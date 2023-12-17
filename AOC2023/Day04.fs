module AOC2023.Day04

open System
open AOC2023.Models
open AOC2023.CommonIO

let parseCard (s: string) =
    let parts = s.Split ":" |> Array.map _.Trim()

    let cardId =
        parts[0].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.item 1
        |> int

    let numsParts =
        parts[1].Split '|'
        |> Array.map _.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Set.ofArray

    (cardId, numsParts[0] |> Set.map int, numsParts[1] |> Set.map int)

module Part1 =

    let processCard (_, myNs: int Set, winningNs: int Set) =
        match Set.intersect myNs winningNs |> Set.count with
        | 0 -> None
        | n -> pown 2 (n - 1) |> Some

    let compute input =
        input |> List.map parseCard |> List.choose processCard |> List.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =

    type Card =
        { Id: int
          MNums: int Set
          WNums: int Set
          Copies: int
          Winning: int }

    let incrementCardBy n (card: Card) = { card with Copies = card.Copies + n }

    let parseLine (s: string) =
        let parts = s.Split ":" |> Array.map _.Trim()

        let cardId =
            parts[0].Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.item 1
            |> int

        let numsParts =
            parts[1].Split '|'
            |> Array.map _.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map Set.ofArray

        (cardId, numsParts[0] |> Set.map int, numsParts[1] |> Set.map int)


    let mkCard (id, myNs: int Set, winningNs: int Set) =
        let wins = Set.intersect myNs winningNs |> Set.count

        { Id = id
          MNums = myNs
          WNums = winningNs
          Copies = 1
          Winning = wins }

    let processScratchCards (cards: Card list) =
        let rec loop (cards: Card list) acc =
            match cards with
            | [] -> acc
            | head :: rest when head.Winning = 0 -> loop rest (head :: acc)
            | head :: rest ->
                let increased =
                    rest |> List.take head.Winning |> List.map (incrementCardBy head.Copies)

                let rest = List.append increased (List.skip head.Winning rest)
                loop rest (head :: acc)

        loop cards []


    let rec compute input =
        input
        |> List.map parseLine
        |> List.map mkCard
        |> processScratchCards
        |> List.map _.Copies
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day04-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
