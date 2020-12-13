module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.MaterialUI.Core

module Application =
    module N = NewComputeComponent
    module D = Core.Domain
    module W = Web.Domain
    module A = AllFieldsComponent

    type Model =
        { allFields : A.Model
          tabs : N.Model list
          titles : string list
          tabIndex : int }
    type Msg =
        | TabChanged of int
        | TabMsg of N.Msg
        | AllFieldsMsg of A.Msg
        | AddCompute

    let init () =
        { allFields = A.init () |> fst
          tabs = []
          titles = []
          tabIndex = -1 }
        , Cmd.none

    let update msg model =
        match msg with
        | TabMsg (N.DatasetMsg DatasetComponent.Remove) ->
            { model with
                tabIndex = -1;
                tabs = model.tabs
                       |> List.mapi (fun i x -> i, x)
                       |> List.filter (fun (i, _) -> i <> model.tabIndex)
                       |> List.map (fun (_, x) -> x)
                titles = model.titles
                         |> List.mapi (fun i x -> i, x)
                         |> List.filter (fun (i, _) -> i <> model.tabIndex)
                         |> List.map (fun (_, x) -> x) }
            , Cmd.none
        | TabMsg N.Close -> { model with tabIndex = -1 }, Cmd.none
        | AddCompute ->
            { model with
                tabIndex = List.length model.tabs
                titles = model.titles @ [ "Новый расчет" ]
                tabs = model.tabs @ [ N.init () |> fst ] }
            , Cmd.none
        | TabChanged i -> { model with tabIndex = i }, Cmd.none
        | TabMsg cmsg ->
            let cmodel = model.tabs.[model.tabIndex]
            let (rmodel, rcmd) = N.update cmsg cmodel
            { model with
                tabs =
                    model.tabs
                    |> List.mapi (fun i m -> if i = model.tabIndex then rmodel else m) }
            , rcmd |> Cmd.map TabMsg
        | AllFieldsMsg cmsg ->
            let cmodel = model.allFields
            let (rmodel, rcmd) = A.update cmsg cmodel
            { model with allFields = rmodel }
            , rcmd |> Cmd.map AllFieldsMsg

    module M = Fable.MaterialUI.Props
    module F = Core.Domain.Format

    let viewContent model dispatch =
        div [] [
            tabs
                [ M.MaterialProp.Value model.tabIndex
                  M.TabsProp.IndicatorColor M.TabsIndicatorColor.Primary
                  Style [ PaddingLeft "40px" ] ] [
                let viewTab title index =
                    tab [ Style [ TextTransform "none" ]; M.MaterialProp.Label <| str title; OnClick (fun i -> dispatch <| TabChanged index) ]
                yield! (model.titles |> List.mapi (fun i t -> viewTab t i))
                yield iconButton [ OnClick (fun _ -> dispatch AddCompute) ] [
                    img [ Src "plus.svg" ] ] ]

            if model.tabIndex >= 0
                then N.view model.tabs.[model.tabIndex] (fun m -> dispatch (TabMsg m))
                else AllFieldsComponent.view model.allFields (fun m -> dispatch (AllFieldsMsg m)) ]

    open Fable.MaterialUI

    let theme = createMuiTheme [ Palette [
        Primary [ Main indigo.``800`` ]
        Secondary [ Main orange.``600`` ] ] ]

    let view model dispatch =
        muiThemeProvider [ Theme (ProviderTheme.Theme theme) ] [
            viewContent model dispatch ]

open Elmish.HMR

Program.mkProgram Application.init Application.update Application.view
|> Program.withReactBatched "elmish-app"
|> Program.run



