module AOC2023.Day01

open System
open AOC2023.Models
open AOC2023.CommonIO

module Part1 =
    let getCalibrationValue (line: string) =
        let chars = Seq.toArray line
        let first = Seq.find Char.IsDigit chars
        let last = Seq.findBack Char.IsDigit chars
        $"{first}{last}" |> int

    let compute lines =
        lines |> Seq.map getCalibrationValue |> Seq.sum

    let run input =
        let result = input |> compute
        printfn $"Part 1: {result}"


module Part2 =
    let numbers =
        [ ("one", '1')
          ("two", '2')
          ("three", '3')
          ("four", '4')
          ("five", '5')
          ("six", '6')
          ("seven", '7')
          ("eight", '8')
          ("nine", '9') ]
        |> Map

    let matchesNumber (s: string) =
        let numStrings = numbers |> Map.keys

        Seq.tryFind (fun (st: string) -> s.EndsWith(st)) numStrings
        |> Option.bind (fun s -> Map.tryFind s numbers)

    let preparse (chars: char list) =
        let rec loop lst digits acc =
            match lst with
            | [] -> digits |> List.rev
            | head :: rest when Char.IsDigit head -> loop rest (head :: digits) [||]
            | head :: rest ->
                let s = Array.append acc [| head |] |> System.String.Concat

                match matchesNumber s with
                | Some c -> loop rest (c :: digits) [| head |]
                | None -> loop rest digits (Array.append acc [| head |])

        loop chars [] [||]

    let getCalibrationValue (line: string) =
        let chars = Seq.toList line
        let digits = preparse chars
        let first = digits |> List.head
        let last = digits |> List.last
        $"{first}{last}" |> int

    let compute lines =
        lines |> Seq.map getCalibrationValue |> Seq.sum

    let run input =
        let result = input |> compute
        printfn $"Part 2: {result}"


let run (part: Parts) =
    let input = readLines "day01-input.txt"

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
