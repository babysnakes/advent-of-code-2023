module AOC2023.Day16

open System
open AOC2023.Models
open AOC2023.CommonIO

type Direction =
    | Top
    | Bottom
    | Left
    | Right

type OutgoingDirection =
    | Dir of Direction
    | Pair of Direction * Direction

type Beam = Direction * (int * int)

let parse (input: string list) =
    [ for idx1, row in (input |> Seq.indexed) do
          for idx2, cell in row |> Seq.indexed do
              if cell <> '.' then
                  yield ((idx1, idx2), cell) ]
    |> Map

let nextSquare maxIdx dir (idx1, idx2) =
    let nextOrNone coordinates =
        match coordinates with
        | idx1, _ when idx1 < 0 || idx1 > maxIdx -> None
        | _, idx2 when idx2 < 0 || idx2 > maxIdx -> None
        | _ -> Some(dir, coordinates)

    match dir with
    | Top -> nextOrNone (idx1 - 1, idx2)
    | Bottom -> nextOrNone (idx1 + 1, idx2)
    | Left -> nextOrNone (idx1, idx2 - 1)
    | Right -> nextOrNone (idx1, idx2 + 1)

let getDirection dir c =
    match (dir, c) with
    | Top, '|'
    | Bottom, '|' -> Dir dir
    | Left, '|'
    | Right, '|' -> Pair(Top, Bottom)
    | Top, '-'
    | Bottom, '-' -> Pair(Right, Left)
    | Left, '-'
    | Right, '-' -> Dir dir
    | Top, '/' -> Dir Right
    | Bottom, '/' -> Dir Left
    | Left, '/' -> Dir Bottom
    | Right, '/' -> Dir Top
    | Top, '\\' -> Dir Left
    | Bottom, '\\' -> Dir Right
    | Left, '\\' -> Dir Top
    | Right, '\\' -> Dir Bottom
    | _, _ -> failwith $"invalid char: '{c}'"

let traceBeams grid maxIdx (dir: Direction) (loc: int * int) =
    let calcNext = nextSquare maxIdx

    let rec loop (acc: Beam Set) (beams: Beam list) =
        match beams with
        | [] -> acc |> Set.map snd |> Set.count
        | beam :: rest ->
            if Set.contains beam acc then
                loop acc rest
            else
                let newAcc = Set.add beam acc
                let dir, loc = beam

                match Map.tryFind loc grid with
                | None ->
                    match calcNext dir loc with
                    | Some next -> loop newAcc (next :: rest)
                    | None -> loop newAcc rest
                | Some c ->
                    match getDirection dir c with
                    | Dir dir ->
                        match calcNext dir loc with
                        | Some next -> loop newAcc (next :: rest)
                        | None -> loop newAcc rest
                    | Pair(dir1, dir2) ->
                        match (calcNext dir1 loc), (calcNext dir2 loc) with
                        | Some b1, Some b2 -> loop newAcc (b1 :: b2 :: rest)
                        | Some b1, None -> loop newAcc (b1 :: rest)
                        | None, Some b2 -> loop newAcc (b2 :: rest)
                        | None, None -> loop newAcc rest

    loop Set.empty [ (dir, loc) ]

module Part1 =
    let compute input =
        let max = (input |> List.length) - 1
        let parsed = parse input
        traceBeams parsed max Right (0, 0)

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
    let input = readLines "day16-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
