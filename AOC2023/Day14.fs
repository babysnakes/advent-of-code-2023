module AOC2023.Day14

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

let sumColumnLoad length rocks =
    rocks
    |> List.map (function
        | { RockLocation.tip = Rounded; RockLocation.idx1 = idx } -> length - idx
        | _ -> 0)
    |> List.sum

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


// The video linked below helped me figure out loop discovery:
// https://www.youtube.com/watch?v=WCVOBKUNc38&ab_channel=HyperNeutrino
module Part2 =
    let folderNorth (nextFree: int, acc: RockLocation list) rock =
        match rock with
        | { RockLocation.tip = Cube } -> (rock.idx1 + 1, rock :: acc)
        | { RockLocation.tip = Rounded } -> (nextFree + 1, { rock with idx1 = nextFree } :: acc)

    let folderWest (nextFree: int, acc: RockLocation list) rock =
        match rock with
        | { RockLocation.tip = Cube } -> (rock.idx2 + 1, rock :: acc)
        | { RockLocation.tip = Rounded } -> (nextFree + 1, { rock with idx2 = nextFree } :: acc)

    let folderSouth (nextFree: int, acc: RockLocation list) rock =
        match rock with
        | { RockLocation.tip = Cube } -> (rock.idx1 - 1, rock :: acc)
        | { RockLocation.tip = Rounded } -> (nextFree - 1, { rock with idx1 = nextFree } :: acc)

    let folderEast (nextFree: int, acc: RockLocation list) rock =
        match rock with
        | { RockLocation.tip = Cube } -> (rock.idx2 - 1, rock :: acc)
        | { RockLocation.tip = Rounded } -> (nextFree - 1, { rock with idx2 = nextFree } :: acc)

    let tiltDirection groupBy sorter folder n rocks =
        rocks
        |> List.groupBy groupBy
        |> List.map (snd >> sorter)
        |> List.map (List.fold folder (n, []) >> snd)
        |> List.concat

    let tiltNorth = tiltDirection (fun r -> r.idx2) (List.sortBy _.idx1) folderNorth 0
    let tiltWest = tiltDirection (fun r -> r.idx1) (List.sortBy _.idx2) folderWest 0

    let tiltSouth last =
        tiltDirection (fun r -> r.idx2) (List.sortByDescending _.idx1) folderSouth last

    let tiltEast last =
        tiltDirection (fun r -> r.idx1) (List.sortByDescending _.idx2) folderEast last

    let fullCycle index1 index2 rocks =
        rocks
        |> tiltNorth
        |> tiltWest
        |> tiltSouth (index1 - 1)
        |> tiltEast (index2 - 1)

    let cycleUntilLoop index1 index2 rocks =
        let rec loop rocks acc =
            let result = fullCycle index1 index2 rocks

            if acc |> List.contains result then
                let rev = List.rev acc
                let first = List.findIndex (fun i -> i = result) rev
                (first, rev)
            else
                loop result (result :: acc)

        loop rocks [ rocks ]

    let compute input =
        let rocks = parseInput input
        let index1 = Array.length input
        let index2 = input[0] |> String.length
        let loopStart, cycles = cycleUntilLoop index1 index2 rocks

        let computedIndex =
            (1000000000 - loopStart) % ((List.length cycles) - loopStart) + loopStart

        cycles |> List.item computedIndex |> sumColumnLoad index1

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day14-input.txt" |> Seq.toArray

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
