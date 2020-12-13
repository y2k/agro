module Core.Domain

type HillRisk = { angle : double; riskK : double }

// Пар, Соя, Озимая Пшеница, Ячмень, Сахарная свекла
type PlantId = Steam | Soy | WinterWheat | Barley | SugarBeet

type Result = PlantId []

type Plant =
  { name            : PlantId
    pricePerTon     : double
    maxSafeDistance : double
    herbRiskK2018   : double
    herbRiskK2019   : double
    herbRiskK2020   : double
    hillRisk        : HillRisk }

let plants =
    [ { name = Steam;       pricePerTon =     0.0; maxSafeDistance = 999999.0; herbRiskK2018 = 1.0; herbRiskK2019 = 1.0; herbRiskK2020 = 1.0; hillRisk = { angle = 0.0; riskK = 1.0 } }
      { name = Soy;         pricePerTon = 30000.0; maxSafeDistance = 999999.0; herbRiskK2018 = 1.0; herbRiskK2019 = 0.5; herbRiskK2020 = 0.2; hillRisk = { angle = 0.0; riskK = 1.0 } }
      { name = WinterWheat; pricePerTon = 16000.0; maxSafeDistance = 999999.0; herbRiskK2018 = 1.0; herbRiskK2019 = 1.0; herbRiskK2020 = 1.0; hillRisk = { angle = 0.0; riskK = 1.0 } }
      { name = Barley;      pricePerTon = 12000.0; maxSafeDistance = 999999.0; herbRiskK2018 = 1.0; herbRiskK2019 = 1.0; herbRiskK2020 = 1.0; hillRisk = { angle = 0.0; riskK = 1.0 } }
      { name = SugarBeet;   pricePerTon =  3500.0; maxSafeDistance =      2.0; herbRiskK2018 = 1.0; herbRiskK2019 = 0.5; herbRiskK2020 = 0.2; hillRisk = { angle = 3.0; riskK = 1.2 } } ]

let getPlantByName plant =
    plants
    |> List.find (fun x -> x.name = plant)

let getPricePerTon plant =
    getPlantByName plant
    |> fun x -> x.pricePerTon

let productivityOfLand plant prevPlant =
    Map.ofList [
        Steam,       Map.ofList [ Steam,  0.0; Soy,  0.0; WinterWheat,  0.0; Barley,  0.0; SugarBeet,  0.0 ]
        Soy,         Map.ofList [ Steam,  4.0; Soy,  2.0; WinterWheat,  3.0; Barley,  4.0; SugarBeet,  2.0 ]
        WinterWheat, Map.ofList [ Steam,  6.0; Soy,  6.0; WinterWheat,  3.0; Barley,  4.0; SugarBeet,  5.0 ]
        Barley,      Map.ofList [ Steam,  4.0; Soy,  5.0; WinterWheat,  4.0; Barley,  3.0; SugarBeet,  5.0 ]
        SugarBeet,   Map.ofList [ Steam, 40.0; Soy, 50.0; WinterWheat, 60.0; Barley, 60.0; SugarBeet, 30.0 ] ]
    |> Map.find plant
    |> Map.find prevPlant

type SoilFertilityLevel = High | Medium | Low

let soilFertilityLevelMap = Map.ofList [
    Steam,       Map.ofList [ High, 1.2; Medium, 1.0; Low, 0.8 ]
    WinterWheat, Map.ofList [ High, 1.2; Medium, 1.0; Low, 0.8 ]
    Soy,         Map.ofList [ High, 1.2; Medium, 1.0; Low, 0.8 ]
    Barley,      Map.ofList [ High, 1.2; Medium, 1.0; Low, 0.8 ]
    SugarBeet,   Map.ofList [ High, 1.2; Medium, 1.0; Low, 0.8 ] ]

let getSoilFertilityLevelMap plant fertilityLevel =
    soilFertilityLevelMap
    |> Map.find plant
    |> Map.find fertilityLevel

type YearValue =
    { name      : PlantId
      herbicide : bool }

type Field =
    { id        : int
      area      : double
      angle     : double
      fertility : SoilFertilityLevel
      weeds     : bool
      property  : bool
      distance  : double
      cluster   : string
      year2018  : YearValue
      year2019  : YearValue
      year2020  : YearValue }

let defaultFields =
    [|{ id = 01; area = 125.4; angle = 1.0; fertility = Medium; weeds = false; property = false; distance =  0.0; cluster =    "Центр"; year2018 = { name =   SugarBeet; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name = WinterWheat; herbicide = false } }
      { id = 02; area = 154.2; angle = 7.0; fertility = Medium; weeds =  true; property =  true; distance =  0.5; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name = WinterWheat; herbicide = false } }
      { id = 03; area = 205.6; angle = 0.0; fertility = Medium; weeds = false; property =  true; distance =  0.0; cluster =    "Центр"; year2018 = { name =   SugarBeet; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name = WinterWheat; herbicide = false } }
      { id = 04; area =  23.5; angle = 6.0; fertility =   High; weeds = false; property =  true; distance =  0.7; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name = WinterWheat; herbicide = false } }
      { id = 05; area = 245.4; angle = 1.0; fertility = Medium; weeds = false; property = false; distance =  0.0; cluster =    "Центр"; year2018 = { name =   SugarBeet; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name = WinterWheat; herbicide = false } }
      { id = 06; area = 154.2; angle = 2.0; fertility = Medium; weeds = false; property = false; distance =  0.0; cluster =    "Центр"; year2018 = { name =   SugarBeet; herbicide =  true }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name = WinterWheat; herbicide = false } }
      { id = 07; area =  15.4; angle = 8.0; fertility = Medium; weeds = false; property =  true; distance =  0.9; cluster =    "Центр"; year2018 = { name =      Barley; herbicide =  true }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name = WinterWheat; herbicide = false } }
      { id = 08; area =  13.0; angle = 7.0; fertility = Medium; weeds = false; property =  true; distance =  0.8; cluster =    "Центр"; year2018 = { name =      Barley; herbicide =  true }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name = WinterWheat; herbicide = false } }
      { id = 09; area = 140.5; angle = 4.0; fertility =    Low; weeds = false; property =  true; distance =  1.8; cluster =   "Восток"; year2018 = { name =         Soy; herbicide =  true }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 10; area = 165.3; angle = 5.0; fertility =    Low; weeds = false; property =  true; distance =  1.2; cluster =   "Восток"; year2018 = { name =         Soy; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 11; area = 110.2; angle = 4.0; fertility =    Low; weeds = false; property =  true; distance =  1.5; cluster =   "Восток"; year2018 = { name =         Soy; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 12; area =  18.8; angle = 1.5; fertility =   High; weeds = false; property =  true; distance =  0.0; cluster =   "Восток"; year2018 = { name =   SugarBeet; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 13; area =  90.6; angle = 2.0; fertility = Medium; weeds = false; property =  true; distance =  0.0; cluster =   "Восток"; year2018 = { name =   SugarBeet; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 14; area =  68.5; angle = 2.0; fertility = Medium; weeds = false; property =  true; distance =  0.0; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =   SugarBeet; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 15; area =  55.4; angle = 1.0; fertility = Medium; weeds = false; property =  true; distance =  0.0; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =   SugarBeet; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 16; area =  54.8; angle = 1.0; fertility = Medium; weeds = false; property =  true; distance =  0.0; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =   SugarBeet; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 17; area =  19.9; angle = 5.0; fertility = Medium; weeds =  true; property =  true; distance =  3.2; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 18; area = 121.4; angle = 0.0; fertility = Medium; weeds = false; property = false; distance =  2.5; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 19; area =  98.8; angle = 1.0; fertility = Medium; weeds = false; property =  true; distance =  0.3; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 20; area =  80.1; angle = 1.0; fertility =   High; weeds = false; property =  true; distance =  0.9; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 21; area = 254.8; angle = 5.0; fertility = Medium; weeds = false; property =  true; distance = 11.7; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 22; area = 154.2; angle = 4.0; fertility = Medium; weeds = false; property =  true; distance = 12.0; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 23; area =  65.3; angle = 2.0; fertility = Medium; weeds = false; property =  true; distance = 11.5; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 24; area =  45.9; angle = 2.0; fertility = Medium; weeds = false; property =  true; distance =  0.3; cluster =   "Восток"; year2018 = { name =   SugarBeet; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 26; area =  90.0; angle = 5.0; fertility = Medium; weeds = false; property =  true; distance =  9.8; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 27; area = 125.4; angle = 5.0; fertility = Medium; weeds = false; property =  true; distance =  6.8; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 28; area =  65.4; angle = 2.0; fertility = Medium; weeds = false; property =  true; distance =  7.2; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 29; area = 187.5; angle = 3.0; fertility = Medium; weeds = false; property =  true; distance =  6.8; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 30; area = 187.6; angle = 3.0; fertility = Medium; weeds = false; property =  true; distance =  6.5; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 31; area = 190.5; angle = 5.0; fertility = Medium; weeds = false; property =  true; distance =  1.3; cluster =   "Восток"; year2018 = { name =         Soy; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 32; area = 220.3; angle = 6.0; fertility = Medium; weeds = false; property =  true; distance =  2.3; cluster =   "Восток"; year2018 = { name =         Soy; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 33; area =  19.2; angle = 4.0; fertility = Medium; weeds = false; property =  true; distance =  2.4; cluster =   "Восток"; year2018 = { name =         Soy; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 34; area = 151.2; angle = 7.0; fertility = Medium; weeds = false; property =  true; distance =  0.3; cluster =   "Восток"; year2018 = { name =         Soy; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 35; area =   6.6; angle = 8.0; fertility =    Low; weeds = false; property =  true; distance =  2.3; cluster =   "Восток"; year2018 = { name =         Soy; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 36; area =   4.6; angle = 4.0; fertility =    Low; weeds = false; property =  true; distance =  0.6; cluster =   "Восток"; year2018 = { name =         Soy; herbicide = false }; year2019 = { name =      Barley; herbicide = false }; year2020 = { name =       Steam; herbicide = false } }
      { id = 37; area = 154.3; angle = 3.0; fertility =   High; weeds = false; property =  true; distance =  9.8; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 38; area =  23.6; angle = 4.0; fertility =   High; weeds = false; property =  true; distance =  6.8; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 39; area =  29.7; angle = 1.0; fertility =   High; weeds = false; property =  true; distance =  7.5; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 40; area = 125.3; angle = 1.0; fertility =    Low; weeds = false; property =  true; distance =  6.8; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 41; area = 187.0; angle = 2.0; fertility =    Low; weeds = false; property =  true; distance =  6.9; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 42; area = 105.7; angle = 2.0; fertility = Medium; weeds = false; property =  true; distance =  6.7; cluster =  "Дальний"; year2018 = { name =       Steam; herbicide = false }; year2019 = { name = WinterWheat; herbicide = false }; year2020 = { name =         Soy; herbicide = false } }
      { id = 43; area = 155.4; angle = 7.0; fertility = Medium; weeds = false; property =  true; distance =  0.5; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name = WinterWheat; herbicide = false } }
      { id = 44; area =  54.2; angle = 2.0; fertility = Medium; weeds = false; property =  true; distance =  0.6; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name =   SugarBeet; herbicide = false } }
      { id = 45; area = 155.6; angle = 3.0; fertility = Medium; weeds = false; property =  true; distance = 0.32; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name =   SugarBeet; herbicide = false } }
      { id = 46; area =  83.5; angle = 1.0; fertility =   High; weeds = false; property =  true; distance =  0.5; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name =   SugarBeet; herbicide = false } }
      { id = 47; area = 145.4; angle = 2.0; fertility =   High; weeds = false; property =  true; distance =  0.8; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name =   SugarBeet; herbicide = false } }
      { id = 48; area = 184.3; angle = 1.0; fertility =   High; weeds = false; property =  true; distance =  0.7; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name =   SugarBeet; herbicide = false } }
      { id = 49; area = 100.4; angle = 2.0; fertility = Medium; weeds = false; property =  true; distance =  0.8; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name =   SugarBeet; herbicide = false } }
      { id = 50; area = 120.9; angle = 2.0; fertility =    Low; weeds = false; property =  true; distance =  0.9; cluster =    "Центр"; year2018 = { name =      Barley; herbicide = false }; year2019 = { name =       Steam; herbicide = false }; year2020 = { name =   SugarBeet; herbicide = false } }
      { id = 51; area = 175.4; angle = 1.0; fertility =   High; weeds = false; property =  true; distance =  0.3; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 52; area = 150.2; angle = 6.0; fertility =   High; weeds = false; property =  true; distance =  0.3; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 53; area = 155.6; angle = 2.0; fertility = Medium; weeds =  true; property =  true; distance =  2.5; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 54; area =  83.5; angle = 2.0; fertility =    Low; weeds =  true; property = false; distance =  2.1; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 55; area = 145.4; angle = 2.0; fertility =    Low; weeds = false; property =  true; distance =  0.8; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 56; area = 184.3; angle = 1.0; fertility = Medium; weeds = false; property =  true; distance =  0.9; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =         Soy; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 57; area = 100.4; angle = 2.0; fertility = Medium; weeds = false; property =  true; distance =  0.5; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =   SugarBeet; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 58; area = 120.9; angle = 1.0; fertility = Medium; weeds = false; property =  true; distance =  0.1; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =   SugarBeet; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 59; area =  89.6; angle = 0.0; fertility = Medium; weeds = false; property =  true; distance =  0.1; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =   SugarBeet; herbicide = false }; year2020 = { name =      Barley; herbicide = false } }
      { id = 60; area = 220.4; angle = 0.0; fertility = Medium; weeds = false; property =  true; distance =  0.2; cluster = "Слободка"; year2018 = { name = WinterWheat; herbicide = false }; year2019 = { name =   SugarBeet; herbicide = false }; year2020 = { name =      Barley; herbicide = false } } |]

module Format =
    let fertilityToString f =
        match f.fertility with
        | Low -> "Низкое"
        | Medium -> "Среднее"
        | High -> "Высокое"

    let plantToString x =
        match x with
        | Steam -> "Пар"
        | Soy -> "Соя"
        | WinterWheat -> "Озимая пшеница"
        | Barley -> "Ячмень"
        | SugarBeet -> "Сахарная свекла"

    let weedsToString f = if f.weeds then "+" else "-"

    let propertyToString f = if f.property then "Собственность" else "Истекает срок"
