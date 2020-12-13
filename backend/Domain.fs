module Backend.Domain

open Core.Domain

// Коэффициент занижения урожая в зависимости от дальности поля
let computeDistantRiskK plant field =
    let p = getPlantByName plant
    if field.distance > p.maxSafeDistance then 1.2
    else 1.0

// Снижающий коэффициент урожайности в зависимости от использования гербицыдов
let computeHerbicideRisk plant field =
    let p = getPlantByName plant
    let risk =
      if field.year2020.herbicide then p.herbRiskK2020
      elif field.year2019.herbicide then p.herbRiskK2019
      elif field.year2018.herbicide then p.herbRiskK2018
      else 1.0
    1.0 / risk

// Снижающий коэффициент урожайности в зависимости от использования гербицыдов
let computeHillRisk plant field =
    let p = getPlantByName plant
    if field.angle > p.hillRisk.angle then p.hillRisk.riskK else 1.0

// Доход с поля при засеве данной культурой
let computeProfit (fields : Field []) plant fieldId =
    let field = fields.[ fieldId - 1 ]
    let prevPlant = field.year2020.name
    // Урожайность культур, тонны с гектара
    let productivity = productivityOfLand plant prevPlant
    // Коэффициент снижения урожайности
    let fertK = getSoilFertilityLevelMap plant field.fertility
    let weedsK = if field.weeds then 1.3 else 1.0
    let propertyK = if field.property then 1.0 else 1.3
    let distantRiskK = computeDistantRiskK plant field
    let herbRiskK = computeHerbicideRisk plant field
    let hillRiskK = computeHillRisk plant field
    let harvestTon = (productivity / fertK / weedsK / propertyK / distantRiskK / herbRiskK / hillRiskK) * field.area
    let pricePerTon = getPricePerTon plant
    harvestTon * pricePerTon

let compareAllProfit fields (r : Result) =
    let r =
        r
        |> Array.mapi (fun i p -> computeProfit fields p (i + 1))
    r |> Array.sum, r

let computeFullAreaForPlant fields (r : Result) plant =
    Array.zip fields r
    |> Array.filter (fun (_, p) -> p = plant)
    |> Array.sumBy (fun (f, _) -> f.area)

let checkResultForAverage averagePlants fields (r : Result) =
    let actualAries =
        plants
        |> List.map (fun p -> p.name, computeFullAreaForPlant fields r p.name)
    actualAries
    |> List.map (fun (plant, curAverArea) ->
        let averForPlant = averagePlants |> Map.find plant
        curAverArea <= 1.3 * averForPlant && curAverArea >= averForPlant / 1.3)
    |> List.forall id

let checkResultForClusters (fields : Field []) (r : Result) =
    r
    |> Array.mapi (fun i p -> fields.[i], p)
    |> Array.groupBy (fun (f, _) -> f.cluster)
    |> Array.map (fun (c, xs) -> c, xs |> Array.map (fun (_, p) -> p) |> Set.ofArray)
    |> Array.forall (fun (_, xs) -> xs |> Set.toList |> List.length <= 3)

let checkResult averagePlants fields r =
    checkResultForAverage averagePlants fields r && checkResultForClusters fields r

let averageAreaWithPlants fields =
    fields
    |> Array.collect (fun x ->
        [| x.year2018.name, x.area
           x.year2019.name, x.area
           x.year2020.name, x.area |])
    |> Array.groupBy (fun (n, _) -> n)
    |> Array.map (fun (n, xs) -> n, xs |> Array.sumBy (fun (_, a) -> a / 3.0))
    |> Map.ofArray

// let generateSample fields (random : System.Random) =
//     fields
//     |> Array.map (fun _ -> plants.[random.Next plants.Length].name)

let generateSample fields (random : System.Random) =
    let clusters =
        fields
        |> Array.map (fun x -> x.cluster)
        |> Array.distinct
    let allPlants = plants |> List.map (fun x -> x.name) |> List.toArray

    let clusDist =
        clusters
        |> Array.map (fun c ->
            let ips = ref allPlants
            let ps =
                Array.init 3 (fun _ ->
                    let p1 = (!ips).[random.Next (!ips).Length]
                    ips := !ips |> Array.filter (fun p -> p <> p1)
                    if p1 = Steam then [| p1 |] else [| p1; p1; p1 |]
                )
                |> Array.concat
            c, ps)
        |> Map.ofArray

    let result =
        fields
        |> Array.map (fun f ->
            let allowedPlants = Map.find f.cluster clusDist
            allowedPlants.[random.Next allowedPlants.Length]
            )
    result

let computeVariants fields =
    let averagePlants = averageAreaWithPlants fields

    let computeInner randomSeed =
        let random = System.Random randomSeed
        Seq.init 2_000 (fun _ -> generateSample fields random)
        |> Seq.filter (checkResult averagePlants fields)
        |> Seq.map (fun r -> compareAllProfit fields r, r)
        |> Seq.toArray

    let random = System.Random 42
    List.init 4 (fun _ -> async { return computeInner (random.Next ()) })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.concat
    |> Array.sortByDescending (fun (p, _) -> p)
