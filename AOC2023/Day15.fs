module AOC2023.Day15

open System
open AOC2023.Models
open AOC2023.CommonIO

module Part1 =
    let hashFolder state (c: char) =
        c |> (int64 >> (+) state >> (*) 17L >> (fun n -> n % 256L))

    let applyHASH = Seq.fold hashFolder 0L

    let compute (input: string) =
        input.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map applyHASH
        |> Array.sum

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
    let input = readLines "day15-input.txt" |> Seq.toList |> List.head

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
