module AOC2023.Day18

open System
open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser

// Had to use Reddit for this one:
// https://www.reddit.com/r/adventofcode/comments/18l2nk2/2023_day_18_easiest_way_to_solve_both_parts/

type Direction =
    | U
    | D
    | L
    | R

let pUp = pchar 'U' >>% U
let pDown = pchar 'D' >>% D
let pLeft = pchar 'L' >>% L
let pRight = pchar 'R' >>% R
let pDir = pUp <|> pDown <|> pLeft <|> pRight

module Part1 =
    type DigStep = Direction * int32
    type Polygon = (int * int) list

    let pLine: Parser<DigStep, unit> = pDir .>> spaces .>>. pint32 .>> regex ".*"

    let turn dir amount (idx1, idx2) =
        match dir with
        | U -> (idx1 - amount, idx2)
        | D -> (idx1 + amount, idx2)
        | L -> (idx1, idx2 - amount)
        | R -> (idx1, idx2 + amount)

    let mkPolygon steps : Polygon =
        let folder point (dir, amount) = turn dir amount point
        ((0, 0), steps) ||> List.scan folder

    // Shoelace formula:
    // https://www.themathdoctors.org/polygon-coordinates-and-areas/
    let calculateArea (polygon: Polygon) =
        let pairs =
            polygon
            |> List.windowed 2
            |> List.map (fun ps -> (ps |> List.head, ps |> List.item 1))

        let sum1 =
            pairs
            |> List.map (fun (point1, point2) -> (fst point1) * (snd point2))
            |> List.sum

        let sum2 =
            pairs
            |> List.map (fun (point1, point2) -> (snd point1) * (fst point2))
            |> List.sum

        (sum1 - sum2 |> abs) / 2

    let calculatePerimeter (steps: DigStep list) = steps |> List.sumBy snd

    let countDigs (steps: DigStep list) =
        let area = mkPolygon steps |> calculateArea
        // Compensate from measuring area from middle of the dig
        let perimeter = calculatePerimeter steps
        area + (perimeter / 2 + 1)


    let compute input =
        input |> List.map (run pLine >> unwrap) |> countDigs

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    type DigStep = Direction * int64
    type Polygon = (int64 * int64) list

    let str s = pstring s

    let pHex =
        pDir >>. spaces >>. pint32 >>. str " (#" >>. regex "[0-9a-fA-F]+" .>> str ")"

    let mkDirStep (h: string) =
        let mkDir =
            function
            | '0' -> R
            | '1' -> D
            | '2' -> L
            | '3' -> U
            | _ -> failwith "invalid direction"

        (mkDir h[5], Int64.Parse(h[..4], System.Globalization.NumberStyles.HexNumber))

    let turn dir amount (idx1, idx2) =
        match dir with
        | U -> (idx1 - amount, idx2)
        | D -> (idx1 + amount, idx2)
        | L -> (idx1, idx2 - amount)
        | R -> (idx1, idx2 + amount)

    let mkPolygon steps : Polygon =
        let folder point (dir, amount) = turn dir amount point
        ((0L, 0L), steps) ||> List.scan folder

    let calculateArea (polygon: Polygon) =
        let pairs =
            polygon
            |> List.windowed 2
            |> List.map (fun ps -> (ps |> List.head, ps |> List.item 1))

        let sum1 =
            pairs
            |> List.map (fun (point1, point2) -> (fst point1) * (snd point2))
            |> List.sum

        let sum2 =
            pairs
            |> List.map (fun (point1, point2) -> (snd point1) * (fst point2))
            |> List.sum

        (sum1 - sum2 |> abs) / 2L

    let calculatePerimeter (steps: DigStep list) = steps |> List.sumBy snd

    let countDigs (steps: DigStep list) =
        let area = mkPolygon steps |> calculateArea
        let perimeter = calculatePerimeter steps
        area + (perimeter / 2L + 1L)

    let compute input =
        input |> List.map (run pHex >> unwrap >> mkDirStep) |> countDigs

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day18-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
