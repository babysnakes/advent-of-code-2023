module AOC2023.Day06

open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser

module Part1 =
    let str s = pstring s
    let pTime = str "Time:" >>. spaces >>. sepEndBy pint32 spaces
    let pDistance = str "Distance:" >>. spaces >>. sepEndBy pint32 spaces

    let times input = run pTime input |> unwrap
    let distances input = run pDistance input |> unwrap

    let betterResults (time, best) =
        [ 1..time ] |> List.filter (fun n -> (time - n) * n > best) |> List.length

    let races input =
        let ts = input |> List.head |> times
        let ds = input |> List.item 1 |> distances
        (ts, ds) ||> List.zip

    let compute input =
        input |> races |> List.map betterResults |> List.reduce (*)

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    let parseLine (s: string) =
        s.Split(':', 2) |> Array.item 1 |> _.Replace(" ", "")

    let parse input = input |> List.map (parseLine >> int64)

    let betterResults (time: int64) (best: int64) =
        let rec loop cur =
            if (time - cur) * cur > best then
                loop (cur - 1L)
            else
                (cur + 1L)

        let half = time / 2L
        let min = loop half
        (time - min) - min + 1L

    let compute input =
        parse input |> (fun l -> (List.head l, List.item 1 l)) ||> betterResults

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day06-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
