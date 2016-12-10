#r "../node_modules/fable-core/Fable.Core.dll"
#r "../node_modules/fable-powerpack/Fable.PowerPack.dll"
#load "../node_modules/fable-arch/Fable.Arch.Html.fs"
#load "../node_modules/fable-arch/Fable.Arch.App.fs"
#load "../node_modules/fable-arch/Fable.Arch.Virtualdom.fs"

open System

open Fable.Core
open Fable.Core.JsInterop

open Fable.Import
open Fable.Import.Browser

open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.PowerPack.Result

open Fable.Arch
open Fable.Arch.Html
open Fable.Arch.App.AppApi

// -----------------------------------------------------------------------------------
// MODEL
//

type Url = string

type Details =
    | Character of name: string
    | Film of title: string * episode: string

type Entity =
    { related : Url list
      details : Details }

type Model =
    | InitialScreen
    | Loading of Entity
    | List of Entity * Entity list
    | ErrorScreen

type ResponseJson =
    { name : string
      title : string
      episode_id : string
      characters : string list
      films : string list }

let parse json =
    let obj = ofJson<ResponseJson> json
    if String.IsNullOrEmpty obj.name then
        { related = obj.characters
          details = Film ( obj.title , obj.episode_id ) }
    else
        { related = obj.films
          details = Character obj.name }

// -----------------------------------------------------------------------------------
// UPDATE
//

type Msg
    = Load of Entity
    | ToList of Entity * Entity list
    | FetchFail

let fetchEntity (url:Url) =
    promise {
        // suddenly gh-pages is https; so I'm just adjusting the API URLs here not to have CORS problems:
        let urlWithoutProtocol = url.Replace("http://", "//")
        let! fetched = fetch urlWithoutProtocol []
        let! response = fetched.text()
        return parse response }

let getFirstCharacter handler =
    promise {
        let! entity = fetchEntity "http://swapi.co/api/people/2/"
        return Load entity }
    |> Promise.map handler
    |> ignore

let getRelatedEntities (entity:Entity) handler =
    List.map fetchEntity entity.related
    |> Promise.Parallel
    |> Promise.map ( fun list -> ToList ( entity , List.ofArray list ) )
    |> Promise.map handler
    |> ignore

let update model msg =
    match msg with
    | Load entity -> Loading entity , [ getRelatedEntities entity ]
    | ToList ( entity , list ) -> List ( entity , list ) , []
    | FetchFail -> ErrorScreen , []

// -----------------------------------------------------------------------------------
// VIEW
//

let bgColor entity =
    match entity.details with
    | Character _ -> "rgba(230, 126, 34,1.0)"
    | Film _ -> "rgba(52, 152, 219,1.0)"

let mainStyle entity =
    Style
        [ "background-color", bgColor entity
          "width", "200px"
          "height", "200px"
          "color", "white"
          "font-family", "-apple-system, system, sans-serif"
          "margin", "20px 0px 0px 20px"
          "cursor", "pointer" ]

let filmNumberStyle =
    Style
        [ "padding", "20px 20px 0px 20px"
          "font-size", "60px" ]

let captionStyle =
    Style
        [ "padding", "20px"
          "font-size", "18px" ]

let filmContents title episode =
    [ div [ filmNumberStyle ] [ text episode ]
      div [ captionStyle ] [ text title ] ]

let characterContents name = [ div [ captionStyle ] [ text name ] ]

let entityView entity =
    let attributes = [ mainStyle entity ; onMouseClick ( fun _ -> entity ) ]
    let contents =
        match entity.details with
        | Film ( title , episode ) -> filmContents title episode
        | Character name -> characterContents name
    div attributes contents

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

let loadingMessageView entity =
    match entity.details with
    | Film ( title , _ ) -> messageView ("Loading " + title + " characters...")
    | Character name -> messageView ("Loading " + name + " films...")

let mappedEntityView entity = Html.map Load ( entityView entity )

let view model =
    match model with
    | InitialScreen ->
        messageView "Loading amazing characters and films..."

    | Loading entity ->
        div [ Style [ "display", "flex" ] ]
            [ mappedEntityView entity ; loadingMessageView entity ]

    | List ( entity , list ) ->
        let listView = List.map mappedEntityView list
        div [ Style [ "display", "flex" ] ]
            [ mappedEntityView entity ; div [] listView ]

    | ErrorScreen ->
        messageView "An error ocurred. Please refresh the page and try again - and may the Force be with you!"

// APP

createApp InitialScreen view update Virtualdom.createRender
|> withStartNodeSelector "#app"
|> withInitMessage getFirstCharacter
|> withSubscriber (fun x -> Fable.Import.Browser.console.log("Event received: ", x))
|> start