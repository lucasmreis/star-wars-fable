#r "../node_modules/fable-core/Fable.Core.dll"
#r "../node_modules/fable-powerpack/Fable.PowerPack.dll"
#load "../node_modules/fable-arch/Fable.Arch.Html.fs"
#load "../node_modules/fable-arch/Fable.Arch.App.fs"
#load "../node_modules/fable-arch/Fable.Arch.Virtualdom.fs"

open Fable.Core.JsInterop

open Fable.Arch
open Fable.Arch.Html
open Fable.Arch.App.AppApi

open Fable.PowerPack
open Fable.PowerPack.Fetch

module Character =
    type Model =
        { name: string
          films: string list }

    let parse = ofJson<Model>

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

module Film =
    type Model =
        { title: string
          episodeId: int
          characters: string list }

    type ModelJSON =
        { title: string
          episode_id: int
          characters: string list }

    let parse str =
        let obj = ofJson<ModelJSON> str
        { title = obj.title
          episodeId = obj.episode_id
          characters = obj.characters }

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

type Model =
    | InitialScreen
    | LoadingFilms of Character.Model
    | LoadingCharacters of Film.Model
    | FilmsFromCharacter of Character.Model * Film.Model list
    | CharactersFromFilm of Film.Model * Character.Model list
    | ErrorScreen

type Msg =
    | LoadCharacters of Film.Model
    | ToCharactersFromFilm of Film.Model * Character.Model list
    | LoadFilms of Character.Model
    | ToFilmsFromCharacter of Character.Model * Film.Model list
    | FetchFail

let fetchEntity url parser =
    promise {
        let! fetched = fetch url []
        let! response = fetched.text()
        return response |> parser }

let getCharacter handler =
    fetchEntity "http://swapi.co/api/people/1/" Character.parse
    |> Promise.map LoadFilms
    |> Promise.catch ( fun _ -> FetchFail )
    |> Promise.map handler
    |> ignore

let getCharacters (film: Film.Model) handler =
    film.characters
    |> List.map ( fun url -> fetchEntity url Character.parse )
    |> Promise.Parallel
    |> Promise.map ( fun chs -> ToCharactersFromFilm (film, List.ofArray chs) )
    |> Promise.catch ( fun _ -> FetchFail )
    |> Promise.map handler
    |> ignore

let getFilms (character: Character.Model) handler =
    character.films
    |> List.map ( fun url -> fetchEntity url Film.parse )
    |> Promise.Parallel
    |> Promise.map ( fun fs -> ToFilmsFromCharacter (character, List.ofArray fs) )
    |> Promise.catch ( fun _ -> FetchFail )
    |> Promise.map handler
    |> ignore

let update model msg =
    match msg with
    | LoadCharacters f ->
        LoadingCharacters f , [ getCharacters f ]

    | ToCharactersFromFilm ( f , chs ) ->
        CharactersFromFilm ( f , chs ), []

    | LoadFilms ch ->
        LoadingFilms ch , [ getFilms ch ]

    | ToFilmsFromCharacter ( ch , fs ) ->
        FilmsFromCharacter ( ch , fs ), []

    | FetchFail ->
        ErrorScreen , []

let messageStyle =
    Style
        [ "margin", "20px 0px 0px 20px"
          "width", "200px"
          "height", "200px"
          "font-family", "-apple-system, system, sans-serif"
          "color", "rgba(149, 165, 166,1.0)"
          "font-size", "18px" ]

let messageView t =
    div [ messageStyle ] [ text t ]

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

createApp InitialScreen view update Virtualdom.createRender
|> withStartNodeSelector "#app"
|> withInitMessage getCharacter
|> start