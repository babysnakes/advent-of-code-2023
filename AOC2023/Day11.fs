module AOC2023.Day11

open AOC2023.Models
open AOC2023.CommonIO

module Part1 =
    type galaxy = { Idx1: int; Idx2: int }

    let parse (input: string seq) length1 length2 =
        seq {
            for idx1 in { 0 .. length1 - 1 } do
                for idx2 in { 0 .. length2 - 1 } do
                    if (Seq.item idx1 input |> Seq.item idx2) = '#' then
                        yield { Idx1 = idx1; Idx2 = idx2 }
        }

    let allPairs galaxies =
        let sortTuple (g1, g2) =
            [ g1; g2 ] |> List.sort |> (fun gs -> (List.head gs, List.item 1 gs))

        List.allPairs galaxies galaxies
        |> List.filter (fun (a, b) -> a <> b)
        |> List.map sortTuple
        |> List.distinct

    let expand1 (galaxies: (galaxy * int) list) idx =
        galaxies
        |> List.map (fun (g, orig) -> if orig > idx then ({ g with Idx1 = g.Idx1 + 1 }, orig) else (g, orig))

    let expand2 (galaxies: (galaxy * int) list) idx =
        galaxies
        |> List.map (fun (g, orig) -> if orig > idx then ({ g with Idx2 = g.Idx2 + 1 }, orig) else (g, orig))

    let findNoGalaxies comparer galaxies length =
        seq {
            for idx in { 0 .. length - 1 } do
                if galaxies |> Seq.forall (comparer idx) then
                    yield idx
        }
        |> Seq.toList

    let findNoGalaxiesIdx1 = findNoGalaxies (fun idx g -> g.Idx1 <> idx)
    let findNoGalaxiesIdx2 = findNoGalaxies (fun idx g -> g.Idx2 <> idx)

    let steps (g1, g2) =
        (abs (g1.Idx1 - g2.Idx1)) + (abs (g1.Idx2 - g2.Idx2))


    let compute input =
        let length1 = Seq.length input
        let length2 = Seq.item 1 input |> Seq.length
        let galaxies = parse input length1 length2 |> List.ofSeq
        let noGalaxiesIdx1 = findNoGalaxiesIdx1 galaxies length1
        let noGalaxiesIdx2 = findNoGalaxiesIdx2 galaxies length2

        let expandedIdx1 =
            noGalaxiesIdx1
            |> List.fold expand1 (galaxies |> List.map (fun g -> (g, g.Idx1)))

        noGalaxiesIdx2
        |> List.fold expand2 (expandedIdx1 |> List.map (fun (g, _) -> (g, g.Idx2)))
        |> List.map fst
        |> allPairs
        |> List.map steps
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    type galaxy = { Idx1: int64; Idx2: int64 }
    let FACTOR = 1000000L

    let parse (input: string seq) length1 length2 =
        seq {
            for idx1 in { 0 .. length1 - 1 } do
                for idx2 in { 0 .. length2 - 1 } do
                    if (Seq.item idx1 input |> Seq.item idx2) = '#' then
                        yield { Idx1 = int64 idx1; Idx2 = int64 idx2 }
        }

    let allPairs galaxies =
        let sortTuple (g1, g2) =
            [ g1; g2 ] |> List.sort |> (fun gs -> (List.head gs, List.item 1 gs))

        List.allPairs galaxies galaxies
        |> List.filter (fun (a, b) -> a <> b)
        |> List.map sortTuple
        |> List.distinct

    // nicer implementation of expand:
    // https://github.com/jovaneyck/advent-of-code-2023/blob/main/day%2011/part1.fsx
    let expandIdx1 empties galaxy =
        let diff = empties |> List.filter (fun i -> i < galaxy.Idx1) |> List.length |> int64
        { galaxy with Idx1 = galaxy.Idx1 + (diff * FACTOR - diff) }

    let expandIdx2 empties galaxy =
        let diff = empties |> List.filter (fun i -> i < galaxy.Idx2) |> List.length |> int64
        { galaxy with Idx2 = galaxy.Idx2 + (diff * FACTOR - diff) }

    let findNoGalaxies comparer galaxies length =
        seq {
            for idx in { 0 .. length - 1 } do
                if galaxies |> Seq.forall (comparer idx) then
                    yield int64 idx
        }
        |> Seq.toList

    let findNoGalaxiesIdx1 = findNoGalaxies (fun idx g -> g.Idx1 <> int64 idx)
    let findNoGalaxiesIdx2 = findNoGalaxies (fun idx g -> g.Idx2 <> int64 idx)

    let steps (g1, g2) =
        (abs (g1.Idx1 - g2.Idx1)) + (abs (g1.Idx2 - g2.Idx2))

    let compute input =
        let length1 = Seq.length input
        let length2 = Seq.item 1 input |> Seq.length
        let galaxies = parse input length1 length2 |> List.ofSeq
        let noGalaxiesIdx1 = findNoGalaxiesIdx1 galaxies length1
        let noGalaxiesIdx2 = findNoGalaxiesIdx2 galaxies length2

        galaxies
        |> List.map (expandIdx1 noGalaxiesIdx1)
        |> List.map (expandIdx2 noGalaxiesIdx2)
        |> allPairs
        |> List.map steps
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day11-input.txt"

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
