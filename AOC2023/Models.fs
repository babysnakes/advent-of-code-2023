﻿module AOC2023.Models

open System
open System.Text.RegularExpressions

type Parts =
    | Part1
    | Part2
    | Unspecified

type Day =
    | Day1
    | Day2
    | Day3
    | Day4
    | Day5
    | Day6
    | Day7
    | Day8
    | Day9
    | Day10
    | Day11
    | Day12
    | Day13
    | Day14
    | Day15
    | Day16
    | Day17
    | Day18
    | Day19

let tryParseInt s =
    try
        s |> int |> Some
    with :? FormatException ->
        None

let parsePart =
    function
    | [] -> Ok Unspecified
    | [ "1" ] -> Ok Part1
    | [ "2" ] -> Ok Part2
    | other -> Error $"Invalid part parameters ({other})"

let parseDay day =
    let rx = Regex(@"(d|day)?(\d\d?)", RegexOptions.Compiled)
    let matches = rx.Match(day)
    let res = matches.Groups[2]

    match res.Success, (tryParseInt res.Value) with
    | true, Some 1 -> Ok Day1
    | true, Some 2 -> Ok Day2
    | true, Some 3 -> Ok Day3
    | true, Some 4 -> Ok Day4
    | true, Some 5 -> Ok Day5
    | true, Some 6 -> Ok Day6
    | true, Some 7 -> Ok Day7
    | true, Some 8 -> Ok Day8
    | true, Some 9 -> Ok Day9
    | true, Some 10 -> Ok Day10
    | true, Some 11 -> Ok Day11
    | true, Some 12 -> Ok Day12
    | true, Some 13 -> Ok Day13
    | true, Some 14 -> Ok Day14
    | true, Some 15 -> Ok Day15
    | true, Some 16 -> Ok Day16
    | true, Some 17 -> Ok Day17
    | true, Some 18 -> Ok Day18
    | true, Some 19 -> Ok Day19
    | true, Some n -> Error $"Invalid day specified ({n})"
    | _, _ -> Error $"Invalid day specified ({day})"
