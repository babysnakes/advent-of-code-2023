module AOC2023.Day02

open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser
open FParsec

type Colour =
    | Blue
    | Green
    | Red

type Set = Map<Colour, int>
type Game = { Id: int; Sets: Set list }

let getColourValue (colour: Colour) (set: Set) =
    set |> Map.tryFind colour |> Option.defaultValue 0

let str s = pstring s
let pGameId = str "Game" >>. spaces >>. pint32 .>> str ":"
let pGreen = spaces >>. str "green" .>> spaces >>% Green
let pRed = spaces >>. str "red" .>> spaces >>% Red
let pBlue = spaces >>. str "blue" .>> spaces >>% Blue
let pColour = seq { pGreen <|> pRed <|> pBlue } |> choice
let pCubeCount = spaces >>. pint32 .>> spaces .>>. pColour

let pSet: Parser<Set, unit> =
    sepBy pCubeCount (str ",") |>> List.map (fun (a, b) -> (b, a)) |>> Map

let pSets = sepBy pSet (str ";")
let pGame = pGameId .>>. pSets |>> (fun (id, sets) -> { Id = id; Sets = sets })

let parseGame (line: string) = run pGame line |> unwrap

module Part1 =
    let maxRed, maxGreen, maxBlue = (12, 13, 14)

    let isPossible (sets: Set list) =
        let setPossible (set: Set) =
            getColourValue Green set <= maxGreen
            && getColourValue Blue set <= maxBlue
            && getColourValue Red set <= maxRed

        sets |> List.forall setPossible

    let compute (input: string list) =
        input
        |> List.map parseGame
        |> List.filter (_.Sets >> isPossible)
        |> List.map _.Id
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"

module Part2 =
    let calculateMaxCubes sets =
        let getMax c =
            sets |> List.maxBy (getColourValue c) |> getColourValue c

        getMax Green * getMax Blue * getMax Red

    let compute (input: string list) =
        input
        |> List.map parseGame
        |> List.map (_.Sets >> calculateMaxCubes)
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2: {result}"

let run (part: Parts) =
    let input = readLines "day02-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
