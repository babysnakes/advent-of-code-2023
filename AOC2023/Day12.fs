module AOC2023.Day12

open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser

type Spring =
    | Operational
    | Damaged
    | Unknown

let str s = pstring s
let pUnknown = pchar '?' >>% Unknown
let pDamaged = pchar '#' >>% Damaged
let pOperational = pchar '.' >>% Operational
let pSpring = pUnknown <|> pDamaged <|> pOperational
let pRecord = many pSpring .>> spaces .>>. sepEndBy pint32 (pchar ',')

module Part1 =
    let isMatchingGroups springs group =
        let folder (cur: int, group: int list) spring =
            match spring with
            | Damaged -> (cur + 1), group
            | _ -> 0, (cur :: group)

        springs
        |> List.fold folder (0, List.empty)
        |> (fun (cur, group) -> cur :: group |> List.filter ((<>) 0) |> List.rev)
        |> (=) group

    let rec calculateAllOptions (springs, group) =
        let rec loop springs =
            match springs with
            | [] -> [ [] ]
            | Operational :: rest -> loop rest |> List.map (fun l -> Operational :: l)
            | Damaged :: rest -> loop rest |> List.map (fun l -> Damaged :: l)
            | Unknown :: rest ->
                let asDamaged = loop rest |> List.map (fun l -> Damaged :: l)
                let asOperational = loop rest |> List.map (fun l -> Operational :: l)
                asDamaged @ asOperational
                
        loop springs |> List.except List.empty |> List.filter (fun lst -> isMatchingGroups lst group) |> List.length

    let compute input =
        input
        |> List.map (fun s -> (run pRecord s |> unwrap))
        |> List.map calculateAllOptions
        |> List.sum

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
    let input = readLines "day12-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
