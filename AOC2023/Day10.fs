module AOC2023.Day10

open System
open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser
open AOC2023.Collections

type TileType =
    | Vert
    | Horiz
    | NE
    | NW
    | SE
    | SW
    | Ground
    | Start

type Tile =
    { Type: TileType; Idx1: int; Idx2: int }

type Direction =
    | N
    | S
    | W
    | E

let pVert = pchar '|' >>% Vert
let pHoriz = pchar '-' >>% Horiz
let pNE = pchar 'L' >>% NE
let pNW = pchar 'J' >>% NW
let pSW = pchar '7' >>% SW
let pSE = pchar 'F' >>% SE
let pGround = pchar '.' >>% Ground
let pStart = pchar 'S' >>% Start
let pOne = pVert <|> pHoriz <|> pNE <|> pNW <|> pSW <|> pSE <|> pGround <|> pStart
let pLine = many pOne
let parseLine s = run pLine s |> unwrap


module Part1 =
    let possibleNeighbors =
        [ (N, [ Vert; NW; NE; Start ])
          (S, [ Vert; SW; SE; Start ])
          (W, [ Horiz; NW; SW; Start ])
          (E, [ Horiz; NE; SE; Start ]) ]
        |> Map

    let isPossibleNeighbor direction tile =
        possibleNeighbors |> Map.find direction |> List.contains tile

    let allNeighbors tile ary =
        let length = Array2D.length1 ary // squares

        let mkTile i1 i2 =
            if (i1 < 0 || i2 < 0) || (i1 >= length || i2 >= length) then
                None
            else
                Some
                    { Idx1 = i1
                      Idx2 = i2
                      Type = (Array2D.get ary i1 i2) }

        let N = mkTile (tile.Idx1 - 1) tile.Idx2
        let S = mkTile (tile.Idx1 + 1) tile.Idx2
        let W = mkTile tile.Idx1 (tile.Idx2 - 1)
        let E = mkTile tile.Idx1 (tile.Idx2 + 1)
        [ N; S; W; E ] |> List.choose id

    let findFromDirection src target =
        match (target.Idx1 - src.Idx1, target.Idx2 - src.Idx2) with
        | 0, -1 -> E
        | 0, 1 -> W
        | -1, 0 -> S
        | 1, 0 -> N
        | invalid -> failwith $"invalid location diff {invalid}"

    let isPath cur next =
        let dir = findFromDirection cur next
        Map.find dir possibleNeighbors |> List.contains next.Type

    let findPossiblePaths tile ary =
        allNeighbors tile ary |> List.filter (fun t -> isPath tile t && isPath t tile)

    let getOtherSide tile from ary =
        findPossiblePaths tile ary |> List.find ((<>) from)

    let followLoop start map =
        let rec loop cur next acc =
            match getOtherSide next cur map with
            | s when s = start -> acc / 2 + 1
            | step -> loop next step (acc + 1)

        let next = findPossiblePaths start map |> List.head
        loop start next 0

    let compute input =
        let aMap = input |> List.map parseLine |> array2D
        let i1, i2 = Array2D.find2D Start aMap |> Option.get
        let start = { Idx1 = i1; Idx2 = i2; Type = Start }
        followLoop start aMap

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
    let input = readLines "day10-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
