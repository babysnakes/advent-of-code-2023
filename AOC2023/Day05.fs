module AOC2023.Day05

open System
open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser

type TypeMappingItem = { Destination: int64; Source: int64; Range: int64 }

let toMappingItem (dest, source, range) =
    { Destination = dest; Source = source; Range = range }

let str s = pstring s
let pSeeds: Parser<int64 list, unit> = str "seeds: " >>. sepEndBy pint64 spaces
let pTitle = regex ".*-to-.* map:" .>> spaces
let pElem = pint64 .>> spaces
let pLine = tuple3 pElem pElem pElem |>> toMappingItem

type Mappings =
    { Seed2Soil: TypeMappingItem list
      Soil2Fertilizer: TypeMappingItem list
      Fertilizer2Water: TypeMappingItem list
      Water2Light: TypeMappingItem list
      Light2Temperature: TypeMappingItem list
      Temperature2Humidity: TypeMappingItem list
      Humidity2Location: TypeMappingItem list }

module Part1 =
    let pSection: Parser<TypeMappingItem list, unit> =
        pTitle >>. sepEndBy pLine spaces |>> List.sortBy _.Source

    let mkMappings (parts: string array) =
        { Seed2Soil = run pSection parts[0] |> unwrap
          Soil2Fertilizer = run pSection parts[1] |> unwrap
          Fertilizer2Water = run pSection parts[2] |> unwrap
          Water2Light = run pSection parts[3] |> unwrap
          Light2Temperature = run pSection parts[4] |> unwrap
          Temperature2Humidity = run pSection parts[5] |> unwrap
          Humidity2Location = run pSection parts[6] |> unwrap }

    let translate (mappings: TypeMappingItem list) (n: int64) : int64 =
        // Assumes mappings are sorted on source.
        let rec loop mappings =
            match mappings with
            | [] -> n
            | head :: rest ->
                if n < head.Source then
                    n
                else if n < head.Source + head.Range then
                    head.Destination + (n - head.Source)
                else
                    loop rest

        loop mappings

    // This could all fit in a list.reduce but it would be less readable
    let getLocation (maps: Mappings) =
        translate maps.Seed2Soil
        >> translate maps.Soil2Fertilizer
        >> translate maps.Fertilizer2Water
        >> translate maps.Water2Light
        >> translate maps.Light2Temperature
        >> translate maps.Temperature2Humidity
        >> translate maps.Humidity2Location

    let compute (input: string) =
        let nl = Environment.NewLine
        let parts = input.Split $"{nl}{nl}"
        let seeds = run pSeeds parts[0] |> unwrap
        let maps = mkMappings (parts |> Array.skip 1)
        seeds |> List.map (getLocation maps) |> List.min

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    type Seed = { Start: int64; Range: int64 }

    let pSection: Parser<TypeMappingItem list, unit> =
        pTitle >>. sepEndBy pLine spaces |>> List.sortBy _.Destination

    let parseSeeds line =
        let ns = run pSeeds line |> unwrap

        ns
        |> List.chunkBySize 2
        |> List.map (fun ls -> { Start = List.head ls; Range = List.item 1 ls })
        |> List.sortBy _.Start

    let findSeed seeds soil =
        let rec loop seeds =
            match seeds with
            | [] -> None
            | head :: _ when head.Start > soil -> None
            | head :: _ when soil <= (head.Start + head.Range) -> Some soil
            | _ :: rest -> loop rest

        loop seeds

    let mkMappings (parts: string array) =
        { Seed2Soil = run pSection parts[0] |> unwrap
          Soil2Fertilizer = run pSection parts[1] |> unwrap
          Fertilizer2Water = run pSection parts[2] |> unwrap
          Water2Light = run pSection parts[3] |> unwrap
          Light2Temperature = run pSection parts[4] |> unwrap
          Temperature2Humidity = run pSection parts[5] |> unwrap
          Humidity2Location = run pSection parts[6] |> unwrap }

    let translate (mappings: TypeMappingItem list) (n: int64) : int64 =
        // Assumes mappings are sorted on destination.
        let rec loop mappings =
            match mappings with
            | [] -> n
            | head :: rest ->
                if n < head.Destination then
                    n
                else if n < head.Destination + head.Range then
                    head.Source + (n - head.Destination)
                else
                    loop rest

        loop mappings

    let getSeed maps seeds loc : int64 option =
        let soil =
            translate maps.Humidity2Location
            >> translate maps.Temperature2Humidity
            >> translate maps.Light2Temperature
            >> translate maps.Water2Light
            >> translate maps.Fertilizer2Water
            >> translate maps.Soil2Fertilizer
            >> translate maps.Seed2Soil

        loc |> soil |> (findSeed seeds)

    let compute (input: string) =
        let nl = Environment.NewLine
        let parts = input.Split $"{nl}{nl}"
        let seeds = parseSeeds parts[0]
        let maps = mkMappings (parts |> Array.skip 1)

        (+) 1
        |> Seq.initInfinite
        |> Seq.find (int64 >> getSeed maps seeds >> Option.isSome)

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readText "day05-input.txt"

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
