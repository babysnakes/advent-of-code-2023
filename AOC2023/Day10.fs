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

type Direction =
    | N
    | S
    | W
    | E

type Tile = { Type: TileType; Idx1: int; Idx2: int }

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

let possibleNeighbors =
    [ (N, [ Vert; NW; NE; Start ])
      (S, [ Vert; SW; SE; Start ])
      (W, [ Horiz; NW; SW; Start ])
      (E, [ Horiz; NE; SE; Start ]) ]
    |> Map

let isPossibleNeighbor direction tile =
    possibleNeighbors |> Map.find direction |> List.contains tile

let allNeighbors tile ary =
    let length1 = Array2D.length1 ary
    let length2 = Array2D.length2 ary

    let mkTile i1 i2 =
        if (i1 < 0 || i2 < 0) || (i1 >= length1 || i2 >= length2) then
            None
        else
            Some { Idx1 = i1; Idx2 = i2; Type = (Array2D.get ary i1 i2) }

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


module Part1 =
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


// Thanks to:
// https://www.reddit.com/r/adventofcode/comments/18fgddy/2023_day_10_part_2_using_a_rendering_algorithm_to/
module Part2 =
    let findStartTileType start ary =
        let ns =
            findPossiblePaths start ary |> List.map (fun t -> findFromDirection t start)

        match ns with
        | [ N; S ]
        | [ S; N ] -> Vert
        | [ N; E ]
        | [ E; N ] -> NE
        | [ N; W ]
        | [ W; N ] -> NW
        | [ S; E ]
        | [ E; S ] -> SE
        | [ S; W ]
        | [ W; S ] -> SW
        | [ E; W ]
        | [ W; E ] -> Horiz
        | _ -> failwith $"invalid start directions: {ns}"

    let followLoop start map =
        let rec loop cur next acc =
            match getOtherSide next cur map with
            | s when s = start -> next :: acc
            | step -> loop next step (next :: acc)

        let next = findPossiblePaths start map |> List.head
        loop start next [ start ]

    let compute input =
        let data = input |> List.map parseLine |> array2D
        let i1, i2 = Array2D.find2D Start data |> Option.get
        let startTile = findStartTileType { Idx1 = i1; Idx2 = i2; Type = Start } data
        let start = { Idx1 = i1; Idx2 = i2; Type = startTile }
        Array2D.set data start.Idx1 start.Idx2 start.Type
        let path = followLoop start data |> Array.ofList

        let pointingNorth t =
            t.Type = Vert || t.Type = NE || t.Type = NW

        let folder (isInside: bool, insides: int) (t: Tile) =
            let contained = Array.contains t path

            if contained && pointingNorth t then (isInside |> not, insides)
            else if isInside && not contained then (isInside, insides + 1)
            else (isInside, insides)

        data
        |> Array2D.mapi (fun i1 i2 v -> { Idx1 = i1; Idx2 = i2; Type = v })
        |> Seq.cast<Tile>
        |> Seq.fold folder (false, 0)
        |> snd

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day10-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
