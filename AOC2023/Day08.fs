module AOC2023.Day08

open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser

type Direction =
    | R
    | L

type Node = { R: string; L: string }
type Network = Map<string, Node>

let followDirection dir node =
    match dir with
    | R -> node.R
    | L -> node.L

let parseNode (k, (l, r)) : string * Node = k, { L = l; R = r }

let pRight = pchar 'R' >>% R
let pLeft = pchar 'L' >>% L
let pDir = pRight <|> pLeft
let pDirections = many pDir
let str s = pstring s
let pName = regex "[A-Z]{3}"

let pNode =
    pName .>> str " = (" .>>. (pName .>> str ", " .>>. pName .>> str ")")
    |>> parseNode

let parseLine s = s |> run pNode |> unwrap

module Part1 =
    let getInstruction (directions: Direction array) idx =
        directions[idx % Array.length directions]

    let compute input =
        let directionStream =
            input
            |> List.head
            |> run pDirections
            |> unwrap
            |> List.toArray
            |> getInstruction

        let nodes = input |> List.skip 2 |> List.map parseLine |> Map

        let rec loop node acc =
            let d = directionStream acc

            match followDirection d node with
            | "ZZZ" -> acc + 1
            | k -> loop (Map.find k nodes) (acc + 1)

        loop (Map.find "AAA" nodes) 0

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
    let input = readLines "day08-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
