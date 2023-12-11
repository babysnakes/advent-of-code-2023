open System
open AOC2023.Models

let printErrorHelp error =
    printf
        $"""
Error: {error}!

Usage: PROGNAME <Day> [ 1 | 2 ]

Day could be one of:
  - number: 1 | 5 | 10
  - d followed by up to two digits: d1 | d01 | d20
  - day followed by up to two digits: day1 | day02 | day21

Optional parameter type is either 1 or 2
"""

let args = Environment.GetCommandLineArgs() |> Array.skip 1 |> List.ofArray

let runDay id part =
    match id with
    | Day1 -> printf $"running day1 with part: {part}"

let run args =
    match args with
    | [] -> printErrorHelp "No day specified!"
    | head :: rest ->
        let day = parseDay head
        let part = parsePart rest
        
        match day, part with
        | Ok d, Ok p -> runDay d p
        | Error err, _ -> printErrorHelp err
        | _, Error err -> printErrorHelp err

run args
