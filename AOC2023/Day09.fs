module AOC2023.Day09

open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser

let pLine = sepEndBy pint32 spaces
let parseLine s = run pLine s |> unwrap

module Part1 =
    type State = int * int list

    let buildUp (zeros: int list) (firsts: int list) : int =
        let generator (cur: int, steps: int list) : (int * State) option =
            match steps with
            | [] -> None
            | head :: rest ->
                let next = cur + head
                (next, (next, rest)) |> Some

        let folder steps head =
            head :: ((head, steps) |> List.unfold generator)

        firsts |> List.fold folder zeros |> List.last

    let processHistory (h: int32 list) : int32 =
        let rec loop (cur: int list) (firsts: int list) =
            let parsed =
                cur |> List.windowed 2 |> List.map (fun l -> List.item 1 l - List.head l)

            if List.forall ((=) 0) parsed then
                buildUp (0 :: parsed) (List.head cur :: firsts)
            else
                loop parsed ((List.head cur) :: firsts)

        loop h []

    let compute input =
        input |> List.map parseLine |> List.map processHistory |> List.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    let processHistory (h: int32 list) =
        let rec loop (cur: int list) (firsts: int list) =
            let parsed =
                cur |> List.windowed 2 |> List.map (fun l -> List.item 1 l - List.head l)

            if List.forall ((=) 0) parsed then
                (List.head cur :: firsts) |> List.fold (fun x y -> y - x) 0
            else
                loop parsed ((List.head cur) :: firsts)

        loop h []

    let compute input =
        input |> List.map parseLine |> List.map processHistory |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day09-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
