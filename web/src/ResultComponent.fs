module ResultComponent

open Elmish
open Fable.React
open Fable.React.Props
open Fable.MaterialUI.Core

module D = Core.Domain
module W = Web.Domain

type Item =
    { id : int
      area : double
      angle : double
      recomendedPlant : D.PlantId
      profit : double }

type Model = { items : Item list }
type Msg =
    | Delete
    | Export

let init () = { items = [] } , Cmd.none

let update msg model =
    match msg with
    | Delete -> { model with items = [] }, Cmd.none
    | Export -> failwith "FIXME"

module M = Fable.MaterialUI.Props
module F = Core.Domain.Format

let private viewItem dispatch (f : Item) =
    let viewCell style content =
        tableCell [ Style ([ Padding "6px" ] @ style); M.TableCellProp.Align M.TableCellAlign.Center ] content
    tableRow [ Key (string f.id) ] [
        viewCell [] [ str <| string f.id ]
        viewCell [] [ str <| string f.area ]
        viewCell [] [ str <| sprintf "%g°" f.angle ]
        viewCell [] [ str <| F.plantToString f.recomendedPlant ]
        viewCell [] [ str <| sprintf "%g ₽" (floor f.profit) ] ]

let private computeProfitSum model =
    if List.isEmpty model.items
        then ""
        else
            model.items
            |> List.sumBy (fun x -> x.profit)
            |> floor
            |> sprintf "(%g ₽)"

let view model dispatch =
        div [ Style
                [ Background "#F8F9FF";
                  Width "1405px"
                  Position PositionOptions.Absolute
                  Left "0px"; Right "0px"; Top "100px"; Bottom "0px"
                  Padding "20px 40px" ] ] [
            div [ Style [ Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center ] ] [
                label [ Style [ FontFamily "Roboto"; Color "#3258DC"; FontSize "16px"; FontWeight "bold" ] ] [
                    str "Расчет от 13.04.2020" ]

                div [ Style [ FlexGrow 1 ] ] [] ]

            table [ Style [ Background "white"; MarginTop "18px" ] ] [
                tableHead [] [
                    tableRow [ Style [ Background "#E9ECFF" ] ] [
                        tableCell [] [ str "Поле" ]
                        tableCell [] [ str "S, Га" ]
                        tableCell [] [ str "Уклон" ]
                        tableCell [] [ str "Рекомендованная культура" ]
                        tableCell [] [ str <| sprintf "Прибыль %s" (computeProfitSum model) ] ] ]
                tableBody [] (model.items |> List.map (viewItem dispatch)) ]

            fab [
                Style
                    [ TextTransform "none"
                      Background "white"
                      Position PositionOptions.Fixed
                      Bottom "32px"; Right "32px" ]
                OnClick (fun _ -> dispatch Export)
                M.FabProp.Variant M.FabVariant.Extended ] [ str "Экспорт данных" ] ]
