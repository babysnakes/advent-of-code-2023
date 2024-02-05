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
    let sprigs2DamagedLayout springs =
        let folder (cur: int, group: int list) spring =
            match spring with
            | Damaged -> (cur + 1), group
            | _ -> 0, (cur :: group)

        springs
        |> List.fold folder (0, List.empty)
        |> (fun (cur, group) -> cur :: group |> List.filter ((<>) 0))

    let damagedLayoutPartialMatching springs damagedLayout =
        sprigs2DamagedLayout springs
        |> fun dl -> if List.isEmpty dl then dl else (dl |> List.tail) |> List.rev
        |> fun dl ->
            if List.length damagedLayout >= List.length dl then
                dl = (damagedLayout |> List.take (List.length dl))
            else
                false

    let damagedLayoutMatching springs group =
        sprigs2DamagedLayout springs |> List.rev |> (=) group

    let rec findPossibleArrangements (springs, damagedLayout) =
        let rec loop springs acc =
            if damagedLayoutPartialMatching (acc |> List.rev) damagedLayout then
                match springs with
                | [] ->
                    let l = acc |> List.rev
                    if damagedLayoutMatching l damagedLayout then [ l ] else [ [] ]
                | Operational :: rest -> loop rest (Operational :: acc)
                | Damaged :: rest -> loop rest (Damaged :: acc)
                | Unknown :: rest ->
                    let asDamaged = loop rest (Damaged :: acc)
                    let asOperational = loop rest (Operational :: acc)
                    asDamaged @ asOperational
            else
                [ [] ]

        loop springs List.empty |> List.filter (List.isEmpty >> not) |> List.length

    let compute input =
        input
        |> List.map (fun s -> (run pRecord s |> unwrap))
        |> List.map findPossibleArrangements
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    open System.Collections.Generic

    type Cache = Dictionary<Spring list * int list, int64>

    // Idea from: https://github.com/jovaneyck/advent-of-code-2023/blob/main/day%2012/part2.fsx
    let rec findPossibleLayouts (springs, damagedLayout) =
        let cache = Cache()
        let oneOf first second s = s = first || s = second

        let rec loop sps dl =
            let key = (sps, dl)

            match cache.TryGetValue(key) with
            | true, result -> result
            | false, _ ->
                let result =
                    match dl with
                    | [] -> if sps |> List.forall (oneOf Operational Unknown) then 1L else 0L
                    | n :: restDl ->
                        match sps with
                        | Operational :: restSps -> loop restSps dl
                        | Damaged :: restSps when restSps |> List.length >= (n - 1) ->
                            let maybeDamaged, other = restSps |> List.splitAt (n - 1)
                            let allDamaged = maybeDamaged |> List.forall (oneOf Damaged Unknown)

                            if allDamaged then
                                match other with
                                | [] -> loop [] restDl
                                | Damaged :: _ -> 0L
                                | Unknown :: rs -> loop rs restDl
                                | Operational :: rs -> loop rs restDl
                            else
                                0L
                        | Unknown :: restSps ->
                            let asDamaged = loop (Damaged :: restSps) dl
                            let asOperational = loop (Operational :: restSps) dl
                            asDamaged + asOperational
                        | _ -> 0L

                cache.Add(key, result)
                result

        loop springs damagedLayout

    let replicate5 (springs: Spring list, damagedLayout: int32 list) =
        let newSprings = springs :: (List.replicate 4 (Unknown :: springs)) |> List.concat
        let newDL = damagedLayout |> List.replicate 5 |> List.concat
        (newSprings, newDL)

    let compute input =
        input
        |> List.map (run pRecord >> unwrap)
        |> List.map replicate5
        |> List.map findPossibleLayouts
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day12-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
