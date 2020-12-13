module NewComputeComponent

open Elmish

module D = Core.Domain
module W = Web.Domain
module F = Core.Domain.Format
module S = DatasetComponent
module R = ResultComponent

type Model =
    { dataset : S.Model
      result : R.Model
      tabIndex : int
      isBusy : bool }

type Msg =
    | TabChanged of int
    | DatasetMsg of S.Msg
    | ResultMsg of R.Msg
    | Close
    | RunComputation
    | RunComputationEnd of W.Sample

let init () =
    { dataset = S.init () |> fst
      result = R.init () |> fst
      tabIndex = 0
      isBusy = false }
    , Cmd.none

let convertResult (fields : D.Field list) (s : W.Sample) : R.Item list =
    Array.zip3
        (fields |> List.toArray)
        s.plants
        s.profit
    |> Array.map (fun (f, plant, profit) ->
        { R.Item.id = f.id
          R.Item.area = f.area
          R.Item.angle = f.angle
          R.Item.recomendedPlant = plant
          R.Item.profit = profit } )
    |> List.ofArray

let update msg model =
    match msg with
    | RunComputation ->
        { model with result = { items = [] }; isBusy = true }
        , Cmd.OfAsync.perform W.sendForm model.dataset.items RunComputationEnd
    | RunComputationEnd r ->
        printfn "RESULT = %A" r
        { model with isBusy = false; result = { items = convertResult model.dataset.items r } }, Cmd.none
    | TabChanged i -> { model with tabIndex = i }, Cmd.none
    | DatasetMsg cmsg ->
        let (rmodel, rcmd) = S.update cmsg model.dataset
        { model with dataset = rmodel }
        , rcmd |> Cmd.map DatasetMsg
    | ResultMsg cmsg ->
        let (rmodel, rcmd) = R.update cmsg model.result
        { model with result = rmodel }
        , rcmd |> Cmd.map ResultMsg
    | Close -> model, Cmd.none

open Fable.MaterialUI.Props
open Fable.React
open Fable.React.Props
open Fable.MaterialUI.Core

let view model dispatch =
    div [ Style [ Width "1480px" ] ] [
        tabs
            [ MaterialProp.Value model.tabIndex
              TabsProp.Variant TabsVariant.Standard
              IndicatorColor TabsIndicatorColor.Secondary
              Style [ PaddingLeft "40px"; Background "#3258DC"; Color "white" ] ] [
            let viewTab title index =
                tab [ Style [ TextTransform "none" ]; MaterialProp.Label <| str title; OnClick (fun i -> dispatch <| TabChanged index) ]
            viewTab "Датасет" 0
            viewTab "Результаты расчетов" 1
            if model.isBusy
                then
                    circularProgress [
                        Style [
                            Color "white"
                            MarginLeft "40px"; Width "20px"; Height "20px";
                            AlignSelf AlignSelfOptions.Center ] ]
                else
                    chip [
                        OnClick (fun _ -> dispatch RunComputation)
                        Style [ AlignSelf AlignSelfOptions.Center; Color "white"; MarginLeft "16px"; Background "white"; Color "#3258DC" ]
                        MaterialProp.Label <| str "Расчитать" ]
            div [ Style [ FlexGrow 1 ] ] []
            chip [
                OnClick (fun _ -> dispatch Close)
                Style [ MarginRight "56px"; AlignSelf AlignSelfOptions.Center; Color "white"; MarginLeft "16px"; Background "white"; Color "#3258DC" ]
                MaterialProp.Label <| str "Закрыть" ] ]

        if model.tabIndex = 0
            then S.view model.dataset (DatasetMsg >> dispatch)
            else R.view model.result (ResultMsg >> dispatch) ]
