module AOC2023.Day14

open System
open AOC2023.Models
open AOC2023.CommonIO

type Rock =
    | Rounded
    | Cube

type RockLocation = { tip: Rock; idx1: int; idx2: int }

let parseInput (lines: string array) =
    seq {
        for idx1 in { 0 .. (lines |> Array.length) - 1 } do
            for idx2 in { 0 .. (lines[0] |> String.length) - 1 } do
                let c = lines[idx1][idx2]

                if c = 'O' then
                    yield { tip = Rounded; idx1 = idx1; idx2 = idx2 }
                else if c = '#' then
                    yield { tip = Cube; idx1 = idx1; idx2 = idx2 }
    }
    |> Seq.toList

module Part1 =
    let rollColumnNorth (rocks: RockLocation list) =
        let folder (nextFree: int, acc: RockLocation list) rock =
            match rock with
            | { RockLocation.tip = Cube } -> (rock.idx1 + 1, rock :: acc)
            | { RockLocation.tip = Rounded } -> (nextFree + 1, { rock with idx1 = nextFree } :: acc)

        rocks |> List.fold folder (0, [])

    let mkDish rocks =
        rocks
        |> List.groupBy _.idx2
        |> Map
        |> Map.map (fun _ v -> v |> List.sortBy (fun r -> r.idx1))

    let tiltDish rocks =
        rocks
        |> Map.map (fun _ v -> v |> rollColumnNorth |> snd)
        |> Map.values
        |> Seq.toList

    let sumColumnLoad length rocks =
        rocks
        |> List.map (function
            | { RockLocation.tip = Rounded; RockLocation.idx1 = idx } -> length - idx
            | _ -> 0)
        |> List.sum

    let compute input =
        input
        |> parseInput
        |> mkDish
        |> tiltDish
        |> List.map (sumColumnLoad (Array.length input))
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =

    let compute input = 0

    let run input =
        let result = compute input
        printfn $"Part2: {result}"
        printfn "NOT IMPLEMENTED"


let run (part: Parts) =
    let input = readLines "day14-input.txt" |> Seq.toArray

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
