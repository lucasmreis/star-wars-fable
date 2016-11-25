#r "../node_modules/fable-core/Fable.Core.dll"
#load "../node_modules/fable-arch/Fable.Arch.Html.fs"
#load "../node_modules/fable-arch/Fable.Arch.App.fs"
#load "../node_modules/fable-arch/Fable.Arch.Virtualdom.fs"

open Fable.Import.Browser
open Fable.Arch
open Fable.Arch.Html
open Fable.Arch.App.AppApi

type Model = {
    title: string
    episodeId: int
    characters: string list }

let mainStyle =
    Style [
        "background-color", "rgba(52, 152, 219,1.0)"
        "width", "200px"
        "height", "200px"
        "color", "white"
        "font-family", "-apple-system, system, sans-serif"
        "margin", "20px 0px 0px 20px"
        "cursor", "pointer" ]

let nameStyle =
    Style [
        "padding", "20px"
        "font-size", "18px" ]

let numberStyle =
    Style [
        "padding", "20px 20px 0px 20px"
        "font-size", "60px" ]

let view model =
    div [
        mainStyle
        onMouseClick (fun _ -> model)
    ] [
        div [ numberStyle ] [ text (model.episodeId.ToString()) ]
        div [ nameStyle ] [ text model.title ] ]

// MAIN

let init:Model = {
    title = "Um Filme"
    episodeId = 123
    characters = ["as"; "df"; "gh"; "jk"] }

let update model msg = model, []

createApp init view update Virtualdom.createRender
|> withStartNodeSelector "#app"
|> start