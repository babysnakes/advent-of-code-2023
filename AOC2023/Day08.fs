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
let pName = regex @"\w{3}"

let pNode =
    pName .>> str " = (" .>>. (pName .>> str ", " .>>. pName .>> str ")")
    |>> parseNode

let parseLine s = s |> run pNode |> unwrap

let getInstruction (directions: Direction array) idx =
    directions[idx % Array.length directions]

module Part1 =
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
    // Adoption from rosetta stone: https://rosettacode.org/wiki/Least_common_multiple#F#
    let rec gcd (a: int64) (b: int64) =
        match a, b with
        | x, 0L -> abs x
        | x, y -> gcd y (x % y)

    let lcm x y = x * y / (gcd x y)

    let followPath directions nodes start =
        let directionStream = directions |> List.toArray |> getInstruction

        let rec loop node acc =
            let d = directionStream acc
            let next = followDirection d node
            if next.EndsWith("Z") then acc + 1 else loop (Map.find next nodes) (acc + 1)

        loop (Map.find start nodes) 0

    let compute input =
        let directions = input |> List.head |> run pDirections |> unwrap
        let nodes = input |> List.skip 2 |> List.map parseLine |> Map

        nodes
        |> Map.keys
        |> Seq.filter (fun s -> s.EndsWith("A"))
        |> List.ofSeq
        |> List.map (followPath directions nodes >> int64)
        |> List.reduce lcm

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day08-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
