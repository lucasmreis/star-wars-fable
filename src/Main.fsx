#r "../node_modules/fable-core/Fable.Core.dll"
#load "../node_modules/fable-arch/Fable.Arch.Html.fs"
#load "../node_modules/fable-arch/Fable.Arch.App.fs"
#load "../node_modules/fable-arch/Fable.Arch.Virtualdom.fs"

open Fable.Arch
open Fable.Arch.Html
open Fable.Arch.App.AppApi

// ---------------------------------------------------------------------------

module Film =
    type Model =
        { title: string
          episodeId: int
          characters: string list }

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
            [ mainStyle ]
            [ div [ numberStyle ] [ text (model.episodeId.ToString()) ]
              div [ nameStyle ] [ text model.title ] ]

// ---------------------------------------------------------------------------

module Character =
    type Model =
        { name: string
          films: string list }

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
            [ mainStyle; onMouseClick (fun _ -> model) ]
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

let initChar:Character.Model =
    { name = "Personagem"
      films = ["as"; "df"; "gh"; "jk"] }

let initFilm:Film.Model =
    { title = "Filme"
      episodeId = 7
      characters = ["as"; "df"; "gh"; "jk"] }

let init = LoadingFilms initChar

// UPDATE

type Msg
    = LoadCharacters of Film.Model
    | ToCharactersFromFilm of Film.Model * Character.Model list
    | LoadFilms of Character.Model
    | ToFilmsFromCharacter of Character.Model * Film.Model
    | FetchFail

let update model msg = model, []

// VIEW

let loadingStyle =
    Style
        [ "margin", "20px 0px 0px 20px"
          "width", "200px"
          "height", "200px"
          "font-family", "-apple-system, system, sans-serif"
          "color", "rgba(149, 165, 166,1.0)"
          "font-size", "18px" ]

let loadingView t =
    div [ loadingStyle ] [ text t ]

let view model =
    match model with
    | InitialScreen ->
        loadingView "Loading amazing characters and films..."

    | LoadingFilms ch ->
        div [ Style [ "display", "flex" ] ]
            [ Character.view ch
              loadingView ("Loading " + ch.name + " films...") ]

    | _ -> div [] []

// APP

createApp init view update Virtualdom.createRender
|> withStartNodeSelector "#app"
|> start