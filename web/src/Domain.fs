module Web.Domain

module D = Core.Domain

type Sample =
    { profit: double []
      plants: D.PlantId [] }

open Fetch
open Fable.Core

let private parsePlant p =
    match p with
    | "Steam" -> D.Steam
    | "Soy" -> D.Soy
    | "WinterWheat" -> D.WinterWheat
    | "Barley" -> D.Barley
    | "SugarBeet"-> D.SugarBeet
    | _ -> failwithf "can't parse %s" p

let sendForm (form: D.Field list) =
    async {
        let body = form |> List.toArray |> JS.JSON.stringify

        let! r =
            fetch "/api" [ Method HttpMethod.POST; Body(U3.Case3 body) ]
            |> Async.AwaitPromise

        let! text = r.text () |> Async.AwaitPromise

        let result =
            JS.JSON.parse text :?> {| profit : double []; plants : string [] |}
            |> fun x -> { profit = x.profit; plants = x.plants |> Array.map parsePlant }

        return result
    }
