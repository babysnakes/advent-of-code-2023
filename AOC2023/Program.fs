open System
open AOC2023
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

let measureTime f p =
    printfn ""
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    f p
    printfn ""
    printf $"Time elapsed: %i{timer.ElapsedMilliseconds} millis"

let runDay id part =
    match id with
    | Day1 -> measureTime Day01.run part
    | Day2 -> measureTime Day02.run part
    | Day3 -> measureTime Day03.run part
    | Day4 -> measureTime Day04.run part
    | Day5 -> measureTime Day05.run part

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
