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
            [ mainStyle
              onMouseClick (fun _ -> model) ]
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

let initChar:Character.Model =
    { name = "Personagem"
      films = ["as"; "df"; "gh"; "jk"] }

let initFilm:Film.Model =
    { title = "Filme"
      episodeId = 7
      characters = ["as"; "df"; "gh"; "jk"] }

let init = (initChar, initFilm)

let update model msg = model, []

let view (ch, film) =
    let chView = Character.view ch
    let filmView = Film.view film
    div [] [ chView ; filmView ]

createApp init view update Virtualdom.createRender
|> withStartNodeSelector "#app"
|> start