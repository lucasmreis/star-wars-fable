#r "../node_modules/fable-core/Fable.Core.dll"
#r "../node_modules/fable-powerpack/Fable.PowerPack.dll"
#load "../node_modules/fable-arch/Fable.Arch.Html.fs"
#load "../node_modules/fable-arch/Fable.Arch.App.fs"
#load "../node_modules/fable-arch/Fable.Arch.Virtualdom.fs"

open System
open Fable.Import.Browser
open Fable.Core.JsInterop

open Fable.PowerPack
open Fable.PowerPack.Fetch

open Fable.Arch
open Fable.Arch.Html
open Fable.Arch.App.AppApi

// ---------------------------------------------------------------------------

module Film =
    type Model =
        { title: string
          episodeId: int
          characters: string list }

    type ModelJSON =
        { title: string
          episode_id: int
          characters: string list }

    let parse (obj:ModelJSON) =
        try
            Some
                { title = obj.title
                  episodeId = obj.episode_id
                  characters = obj.characters }
        with _ -> None


    let mainStyle =
        Style
            [ "background-color", "rgba(52, 152, 219,1.0)"
              "width", "200px"
              "height", "200px"
              "color", "white"
              "font-family", "-apple-system, system, sans-serif"
              "margin", "20px 0px 0px 20px"
              "cursor", "pointer" ]

    let nameStyle =
        Style
            [ "padding", "20px"
              "font-size", "18px" ]

    let numberStyle =
        Style
            [ "padding", "20px 20px 0px 20px"
              "font-size", "60px" ]

    let view model =
        div
            [ mainStyle ; onMouseClick (fun _ -> model) ]
            [ div [ numberStyle ] [ text (model.episodeId.ToString()) ]
              div [ nameStyle ] [ text model.title ] ]

// ---------------------------------------------------------------------------

module Character =
    type Model =
        { name: string
          films: string list }

    type ModelJSON = Model

    let parse (obj:ModelJSON):Model option =
        try
            Some
                { name = obj.name
                  films = obj.films }
        with _ -> None

    let mainStyle =
        Style
            [ "background-color", "rgba(230, 126, 34,1.0)"
              "width", "200px"
              "height", "200px"
              "color", "white"
              "font-family", "-apple-system, system, sans-serif"
              "margin", "20px 0px 0px 20px"
              "cursor", "pointer" ]


    let nameStyle =
        Style
            [ "padding", "20px"
              "font-size", "18px" ]

    let view model =
        div
            [ mainStyle ; onMouseClick (fun _ -> model) ]
            [ div [ nameStyle ] [ text model.name ] ]


// ---------------------------------------------------------------------------
// MAIN
//

type Model =
    | InitialScreen
    | LoadingFilms of Character.Model
    | LoadingCharacters of Film.Model
    | FilmsFromCharacter of Character.Model * Film.Model list
    | CharactersFromFilm of Film.Model * Character.Model list
    | ErrorScreen

let initChar:Character.Model =
    { name = "Personagem"
      films = ["as"; "df"; "gh"; "jk"] }

let initFilm:Film.Model =
    { title = "Filme"
      episodeId = 7
      characters = ["as"; "df"; "gh"; "jk"] }

// UPDATE

type Msg
    = LoadCharacters of Film.Model
    | ToCharactersFromFilm of Film.Model * Character.Model list
    | LoadFilms of Character.Model
    | ToFilmsFromCharacter of Character.Model * Film.Model list
    | FetchFail

let fetchEntity url jsonFn parseFn =
    promise {
        let! response = fetch(url, [])
        let! body = if response.Ok then response.text() else failwith "http error"
        let json = jsonFn(body)
        return
            match parseFn json with
            | Some entity -> entity
            | None -> failwith "json schema error" }

let getCharacter handler =
    promise {
        try
            let! character = fetchEntity "http://swapi.co/api/people/1/" ofJson<Character.ModelJSON> Character.parse
            return LoadFilms character
        with e ->
            window.console.error("Fetch Error:", e)
            return FetchFail }
    |> Promise.map handler
    |> ignore

let getCharacters (film: Film.Model) handler =
    film.characters
        |> List.map ( fun url -> promise {
            try
                let! character = fetchEntity url ofJson<Character.ModelJSON> Character.parse
                return Some character
            with e ->
                return None } )
        |> Promise.Parallel
        |> Promise.map (
            Array.choose id
            >> Array.toList
            >> fun chs -> ToCharactersFromFilm (film, chs)
            >> handler )
        |> ignore

let getFilms (character: Character.Model) handler =
    character.films
        |> List.map ( fun url -> promise {
            try
                let! film = fetchEntity url ofJson<Film.ModelJSON> Film.parse
                return Some film
            with e ->
                return None } )
        |> Promise.Parallel
        |> Promise.map (
            Array.choose id
            >> Array.toList
            >> fun fs -> ToFilmsFromCharacter (character, fs)
            >> handler )
        |> ignore

let update model msg =
    match msg with
    | LoadCharacters f -> LoadingCharacters f , [ getCharacters f ]
    | ToCharactersFromFilm ( f , chs ) -> CharactersFromFilm ( f , chs ), []
    | LoadFilms ch -> LoadingFilms ch , [ getFilms ch ]
    | ToFilmsFromCharacter ( ch , fs ) -> FilmsFromCharacter ( ch , fs ), []
    | FetchFail -> ErrorScreen , []

// VIEW

let loadingStyle =
    Style
        [ "margin", "20px 0px 0px 20px"
          "width", "200px"
          "height", "200px"
          "font-family", "-apple-system, system, sans-serif"
          "color", "rgba(149, 165, 166,1.0)"
          "font-size", "18px" ]

let messageView t =
    div [ loadingStyle ] [ text t ]

let mappedCharacterView =
    Character.view >> Html.map LoadFilms

let mappedFilmView =
    Film.view >> Html.map LoadCharacters

let view model =
    match model with
    | InitialScreen ->
        messageView "Loading amazing characters and films..."

    | LoadingFilms ch ->
        div [ Style [ "display", "flex" ] ]
            [ mappedCharacterView ch
              messageView ("Loading " + ch.name + " films...") ]

    | FilmsFromCharacter (ch, fs) ->
        let filmsView = List.map mappedFilmView fs
        div [ Style [ "display", "flex" ] ]
            [ mappedCharacterView ch
              div [] filmsView ]

    | LoadingCharacters f ->
        div [ Style [ "display", "flex" ] ]
            [ mappedFilmView f
              messageView ("Loading " + f.title + " characters...") ]

    | CharactersFromFilm (f, chs) ->
        let chsView = List.map mappedCharacterView chs
        div [ Style [ "display", "flex" ] ]
            [ mappedFilmView f
              div [] chsView ]

    | ErrorScreen ->
        messageView "An error ocurred. Please refresh the page and try again - and may the Force be with you!"

// APP

createApp InitialScreen view update Virtualdom.createRender
|> withStartNodeSelector "#app"
|> withInitMessage getCharacter
|> withSubscriber (fun x -> Fable.Import.Browser.console.log("Event received: ", x))
|> start