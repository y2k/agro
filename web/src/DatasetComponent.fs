module DatasetComponent

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
    | Export
    | Remove
    | Compute
    | ComputeEnd of W.Sample
    | ChecboxChanged1 of int
    | ChecboxChanged2 of int
    | ChecboxChanged3 of int

let init () = { items = D.defaultFields |> List.ofArray } , Cmd.none

let update msg model =
    let updateItem id f =
        { model with items = model.items |> List.map (fun x -> if x.id = id then f x else x ) }
        , Cmd.none
    match msg with
    | WeedsChanged (id, value) -> updateItem id (fun x -> { x with weeds = not x.weeds })
    | TextChanged (id, text) -> updateItem id (fun x -> { x with area = float text})
    | ChecboxChanged1 id ->
        updateItem id (fun x -> { x with year2018 = { x.year2018 with herbicide = not x.year2018.herbicide } })
    | ChecboxChanged2 id ->
        updateItem id (fun x -> { x with year2019 = { x.year2019 with herbicide = not x.year2019.herbicide } })
    | ChecboxChanged3 id ->
        updateItem id (fun x -> { x with year2020 = { x.year2020 with herbicide = not x.year2020.herbicide } })
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
    | Export ->
        Browser.Dom.document.location.replace "/result.xlsx"
        model, Cmd.none
    | Compute -> model, Cmd.OfAsync.perform W.sendForm model.items ComputeEnd
    | ComputeEnd _ -> model, Cmd.none

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
        viewCell [] [ switch [ Checked f.weeds; OnChange (fun _ -> dispatch <| WeedsChanged (f.id, false)) ] ]
        viewCell [] [ str <| F.plantToString f.year2018.name ]
        viewCell [] [ switch [ Checked f.year2018.herbicide; OnChange (fun _ -> dispatch <| ChecboxChanged1 f.id) ] ]
        viewCell [] [ str <| F.plantToString f.year2019.name ]
        viewCell [] [ switch [ Checked f.year2019.herbicide; OnChange (fun _ -> dispatch <| ChecboxChanged2 f.id) ] ]
        viewCell [] [ str <| F.plantToString f.year2020.name ]
        viewCell [] [ switch [ Checked f.year2020.herbicide; OnChange (fun _ -> dispatch <| ChecboxChanged3 f.id) ] ] ]

let view model dispatch =
        div [ Style
                [ Background "#F8F9FF";
                  Position PositionOptions.Absolute
                  Width "1405px"
                  Left "0px"; Right "0px"; Top "100px"; Bottom "0px"
                  Padding "20px 40px" ] ] [
            div [ Style [ Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center ] ] [
                label [ Style [ FontFamily "Roboto"; Color "#3258DC"; FontSize "16px"; FontWeight "bold" ] ] [
                    str "Расчет от 13.04.2020" ]

                chip [
                    OnClick (fun _ -> dispatch Remove)
                    Style [ Color "white"; MarginLeft "16px" ]
                    M.MaterialProp.Color M.ComponentColor.Secondary
                    M.MaterialProp.Label <| str "Удалить" ]

                div [ Style [ FlexGrow 1 ] ] [] ]

            table [ Style [ Background "white"; MarginTop "18px" ] ] [
                tableHead [] [
                    tableRow [ Style [ Background "#E9ECFF" ] ] [
                        tableCell [] [ str "Поле" ]
                        tableCell [ Style [ MinWidth "30px" ] ] [ str "S, Га" ]
                        tableCell [] [ str "Уклон" ]
                        tableCell [ Style [ MinWidth "150px" ] ] [ str "Собственность" ]
                        tableCell [ Style [ MinWidth "120px" ] ] [ str "До асфальта (км)" ]
                        tableCell [] [ str "Массив" ]
                        tableCell [ Style [ MinWidth "130px" ] ] [ str "Плодородие" ]
                        tableCell [] [ str "Сорняки" ]
                        tableCell [] [ str "Культура (2018)" ]
                        tableCell [] [ str "Гербицид (2018)" ]
                        tableCell [] [ str "Культура (2019)" ]
                        tableCell [] [ str "Гербицид (2019)" ]
                        tableCell [] [ str "Культура (2020)" ]
                        tableCell [] [ str "Гербицид (2020)" ] ] ]
                tableBody [] (model.items |> List.map (viewItem dispatch)) ]

            fab [
                Style
                    [ TextTransform "none"
                      Background "white"
                      Position PositionOptions.Fixed
                      Bottom "32px"; Right "32px" ]
                OnClick (fun _ -> dispatch Export)
                M.FabProp.Variant M.FabVariant.Extended ] [ str "Экспорт данных" ] ]
