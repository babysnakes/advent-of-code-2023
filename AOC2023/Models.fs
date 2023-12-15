module AOC2023.Models

open System
open System.Text.RegularExpressions

type Parts =
    | Part1
    | Part2
    | Unspecified

type Day =
    | Day1
    | Day2

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
    | true, Some n -> Error $"Invalid day specified ({n})"
    | _, _ -> Error $"Invalid day specified ({day})"
