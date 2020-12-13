module JSON =
    open System.Text.Json
    open System.Text.Json.Serialization

    let private options = JsonSerializerOptions()
    options.Converters.Add(
        JsonFSharpConverter(
            JsonUnionEncoding.BareFieldlessTags))

    let deserialize bytes =
        bytes
        |> UTF8.toString
        |> fun json -> JsonSerializer.Deserialize (json, options)

    let serialize data =
        JsonSerializer.Serialize (data, options)

open System.IO
open Suave
open Suave.Operators
open Suave.Filters

module D = Core.Domain
module B = Backend.Domain

[<EntryPoint>]
let main _ =
    let config = { defaultConfig with
                    homeFolder = Some <| Path.Combine(Directory.GetCurrentDirectory(), "public")
                    bindings = [ HttpBinding.create HTTP (System.Net.IPAddress.Parse "0.0.0.0") 8090us  ] }
    choose [
        GET >=> path "/ping" >=> Successful.OK "pong"
        GET >=> path "/" >=> Files.browseFileHome "index.html"
        GET >=> path "/bundle.js" >=> Files.browseFileHome "bundle.js"
        GET >=> pathRegex ".+\\.(png|svg|xlsx)" >=> Files.browseHome
        POST >=> path "/api" >=> request (fun r ->
            let model : D.Field [] = JSON.deserialize r.rawForm
            let result =
                model
                |> B.computeVariants
                |> Array.map (fun ((_, profit), plants) -> {| profit = profit; plants = plants |})
                |> Array.head
            printfn "RESULT = %A" <| JSON.serialize result
            JSON.serialize result
            |> Successful.OK)
    ]
    |> startWebServer config
    0
