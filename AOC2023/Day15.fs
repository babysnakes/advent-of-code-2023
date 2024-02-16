module AOC2023.Day15

open System
open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser

let hashFolder state (c: char) =
    c |> (int >> (+) state >> (*) 17 >> (fun n -> n % 256))

let applyHASH = Seq.fold hashFolder 0

module Part1 =
    let compute (input: string) =
        input.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map applyHASH
        |> Array.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    type Operation =
        | Minus
        | Equals of int

    type State = Map<int, (string * int) list>

    let pLetters = regex "\w+"
    let parseMinus = pchar '-' >>% Minus
    let parseEquals = pchar '=' >>. pint32 |>> Equals
    let pOperation = parseMinus <|> parseEquals
    let pSequence = pLetters .>>. pOperation

    let lensesFolder (state: State) (label: string, op: Operation) : State =
        let removeLabel lenses =
            match List.tryFindIndex (fun (l, _) -> l = label) lenses with
            | None -> Some lenses
            | Some idx -> lenses |> List.removeAt idx |> Some

        let addLabel lenses focalLength =
            match List.tryFindIndex (fun (l, _) -> l = label) lenses with
            | None -> (label, focalLength) :: lenses |> Some
            | Some idx ->
                lenses
                |> List.mapi (fun i v -> if i = idx then (label, focalLength) else v)
                |> Some


        let change box =
            match (box, op) with
            | None, Minus -> None
            | None, Equals focalLength -> [ (label, focalLength) ] |> Some
            | Some lenses, Minus -> removeLabel lenses
            | Some lenses, Equals focalLength -> addLabel lenses focalLength

        state |> Map.change (applyHASH label) change

    let calculateFocusingPower (box: int) (lenses: (string * int) list) =
        lenses
        |> List.mapi (fun idx (_, forcalLength) -> (box + 1) * (idx + 1) * forcalLength)

    let compute (input: string) =
        input.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (run pSequence >> unwrap)
        |> Array.fold lensesFolder Map.empty
        |> Map.map (fun _ -> List.rev)
        |> Map.map calculateFocusingPower
        |> Map.values
        |> List.concat
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day15-input.txt" |> Seq.toList |> List.head

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
