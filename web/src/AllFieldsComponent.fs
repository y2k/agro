module AllFieldsComponent

open Elmish
open Fable.React
open Fable.React.Props
open Fable.MaterialUI.Core

module D = Core.Domain
module W = Web.Domain

type Model = { items : D.Field list }
type Msg =
    | AddRow
    | TextChanged of int * string
    | WeedsChanged of int * bool
    | Import
    | Compute
    | ComputeEnd of W.Sample []

let init () = { items = D.defaultFields |> Array.toList } , Cmd.none

let update msg model =
    let updateItem id f =
        { model with items = model.items |> List.map (fun x -> if x.id = id then f x else x ) }
        , Cmd.none
    match msg with
    | WeedsChanged (id, value) -> updateItem id (fun x -> { x with weeds = not x.weeds })
    | TextChanged (id, text) -> updateItem id (fun x -> { x with area = float text})
    | AddRow ->
        { model with items =
                        model.items @ [
                            { id = model.items.Length + 1
                              area = 0.0
                              angle = 0.0
                              fertility = D.SoilFertilityLevel.Medium
                              weeds = false
                              property = true
                              distance = 0.0
                              cluster = "Центр"
                              year2018 = { name = D.PlantId.Steam; herbicide = false }
                              year2019 = { name = D.PlantId.Steam; herbicide = false }
                              year2020 = { name = D.PlantId.Steam; herbicide = false } } ] }
        , Cmd.none
    | Import -> { model with items = D.defaultFields |> List.ofArray }, Cmd.none

module M = Fable.MaterialUI.Props
module F = Core.Domain.Format

let private viewItem dispatch (f : D.Field) =
    let getFertIcon (x : D.Field) =
        match x.fertility with
        | D.Low -> "low.svg"
        | D.Medium -> "medium.svg"
        | D.High -> "high.svg"
    let viewCell style content =
        tableCell [ Style ([ Padding "6px" ] @ style); M.TableCellProp.Align M.TableCellAlign.Center ] content
    tableRow [ Key (string f.id) ] [
        viewCell [] [ str <| string f.id ]
        viewCell [] [
            inputBase [
                M.MaterialProp.Value f.area;
                OnChange (fun x -> dispatch <| TextChanged (f.id, x.Value)) ] ]
        viewCell [] [ str <| sprintf "%g°" f.angle ]
        viewCell
            [ Background (if f.property then "white" else "#FCF1E7") ]
            [ div [ Style [ Display DisplayOptions.Flex
                            JustifyContent "center"
                            AlignItems AlignItemsOptions.Center ] ]
                  [ str <| F.propertyToString f
                    if f.property
                        then div [] []
                        else img [ Src "alert.svg"; Style [ MarginLeft "8px"; Width "24px"; Height "24px" ] ] ] ]
        viewCell [] [ str <| string f.distance ]
        viewCell [] [ str <| string f.cluster ]
        viewCell []
            [ div [ Style [ Display DisplayOptions.Flex
                            JustifyContent "space-between"
                            AlignItems AlignItemsOptions.Center
                            Padding "0px 10px" ] ]
                  [ str <| F.fertilityToString f
                    img [ Src (getFertIcon f) ] ] ]
        viewCell [] [ switch [ Checked f.weeds; OnChange (fun _ -> dispatch <| WeedsChanged (f.id, false)) ] ] ]

let view model dispatch =
    div [] [
        div [ Style
                [ Background "#F8F9FF";
                  Position PositionOptions.Absolute
                  Left "0px"; Right "0px"; Top "50px"; Bottom "0px"
                  Padding "20px 40px" ] ] [
            div [ Style [ Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center ] ] [
                label [ Style [ FontFamily "Roboto"; Color "#3258DC"; FontSize "16px"; FontWeight "bold" ] ] [
                    str "Состояние полей на сегодня" ]

                div [ Style [ FlexGrow 1 ] ] []

                chip [
                    OnClick (fun _ -> dispatch AddRow)
                    M.MaterialProp.Color M.ComponentColor.Primary
                    M.MaterialProp.Label <| str "Добавить поле" ]
                chip [
                    Style [ MarginLeft "16px" ]
                    OnClick (fun _ -> ())
                    M.MaterialProp.Color M.ComponentColor.Primary
                    M.MaterialProp.Label <| str "Календарь состояний полей" ] ]

            table [ Style [ Background "white"; MarginTop "18px" ] ] [
                tableHead [] [
                    tableRow [ Style [ Background "#E9ECFF" ] ] [
                        tableCell [] [ str "Поле" ]
                        tableCell [] [ str "S, Га" ]
                        tableCell [] [ str "Уклон" ]
                        tableCell [] [ str "Собственность" ]
                        tableCell [] [ str "До асфальта (км)" ]
                        tableCell [] [ str "Массив" ]
                        tableCell [] [ str "Плодородие" ]
                        tableCell [] [ str "Сорняки" ] ] ]
                tableBody [] (model.items |> List.map (viewItem dispatch)) ]
            // button
            //     [ M.ButtonProp.Variant M.ButtonVariant.Contained
            //       OnClick (fun _ -> dispatch Compute) ]
            //     [ str "Расчитать" ]

            fab [
                Style
                    [ TextTransform "none"
                      Background "white"
                      Position PositionOptions.Fixed
                      Bottom "32px"; Right "32px" ]
                OnClick (fun _ -> dispatch Import)
                M.FabProp.Variant M.FabVariant.Extended ] [ str "Импорт состояния полей" ] ] ]
