module AOC2023.Day13

open System
open AOC2023.Models
open AOC2023.CommonIO

let nl = Environment.NewLine

let parseSection (input: string array) =
    seq {
        for idx1 in { 0 .. (input |> Array.length) - 1 } do
            for idx2 in { 0 .. (input[0] |> String.length) - 1 } do
                let c = input |> Array.item idx1 |> (fun s -> s[idx2])

                if c = '#' then
                    yield (idx1, idx2)
    }
    |> Seq.toList

let mapDirection f1 f2 (rocks: (int * int) list) =
    rocks
    |> List.groupBy f1
    |> List.map (fun (a, b) -> (a, b |> List.map f2))
    |> Map

let mapHorizontally = mapDirection fst snd
let mapVertically = mapDirection snd fst

module Part1 =
    // Note: The result that returns true should be incremented by 1
    let testMirror rocks index =
        let maxMirrorParts = min (index + 1) (Map.count rocks - index - 1)
        let range = if maxMirrorParts = 0 then [ 0 ] else [ 0 .. maxMirrorParts - 1 ]
        let mapFind key = Map.find key rocks
        range |> List.forall (fun n -> mapFind (index - n) = mapFind (index + n + 1))

    let tryDirection rocks length =
        [ length - 2 .. -1 .. 0 ] // go for larger mirrors, works for my input
        |> List.tryFind (testMirror rocks)

    let rec processSection (section: String) =
        let lines = section.Split(nl)
        let horizSize = lines |> Array.length
        let vertSize = lines[0] |> String.length
        let rocks = lines |> parseSection

        let tryHoriz () =
            tryDirection (mapHorizontally rocks) horizSize
            |> Option.map (((+) 1) >> ((*) 100))

        let tryVert () =
            tryDirection (mapVertically rocks) vertSize |> Option.map ((+) 1)

        tryVert () |> Option.orElseWith tryHoriz |> Option.get

    let compute (input: String) =
        input.Split($"{nl}{nl}") |> Array.toList |> List.map processSection |> List.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    // Note: The result that returns true should be incremented by 1
    let testMirror rocks index =
        let maxMirrorParts = min (index + 1) (Map.count rocks - index - 1)
        let range = if maxMirrorParts = 0 then [ 0 ] else [ 0 .. maxMirrorParts - 1 ]
        let mapFind key = Map.find key rocks

        range
        |> List.filter (fun n -> (mapFind (index - n) <> mapFind (index + n + 1)))
        |> List.tryExactlyOne
        |> Option.map (fun n ->
            let left = mapFind (index - n) |> Set.ofList
            let right = mapFind (index + n + 1) |> Set.ofList

            match ((Set.difference left right |> Set.count), (Set.difference right left |> Set.count)) with
            | 0, 1
            | 1, 0 -> true
            | _ -> false)
        |> Option.defaultValue false

    let tryDirection rocks length =
        [ length - 2 .. -1 .. 0 ] // go for larger mirrors, works for my input
        |> List.tryFind (testMirror rocks)

    let rec processSection (section: String) =
        let lines = section.Split(nl)
        let horizSize = lines |> Array.length
        let vertSize = lines[0] |> String.length
        let rocks = lines |> parseSection

        let tryHoriz () =
            tryDirection (mapHorizontally rocks) horizSize
            |> Option.map (((+) 1) >> ((*) 100))

        let tryVert () =
            tryDirection (mapVertically rocks) vertSize |> Option.map ((+) 1)

        tryHoriz () |> Option.orElseWith tryVert |> Option.get


    let compute (input: string) =
        input.Split($"{nl}{nl}") |> Array.toList |> List.map processSection |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readText "day13-input.txt"

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
