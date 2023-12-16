module AOC2023.Day03

open System
open AOC2023.Models
open AOC2023.CommonIO

type Symbol = { T: char; X: int; Y: int }

type Number =
    { Num: int
      StartX: int
      EndX: int
      Y: int }

let parseLine (lineIdx: int) (line: string) : Number list * Symbol array =
    let parseDigits idx (digits: char list) =
        { Num = digits |> Array.ofList |> String.Concat |> int
          StartX = idx
          EndX = idx + (digits |> List.length) - 1
          Y = lineIdx }

    let rec loop (cs: char list) idx symAcc numAcc =
        match cs with
        | [] -> (numAcc |> List.rev, symAcc)
        | '.' :: rest -> loop rest (idx + 1) symAcc numAcc
        | head :: rest when Char.IsDigit head ->
            let digits = head :: rest |> List.takeWhile Char.IsDigit
            let rest = rest |> List.skipWhile Char.IsDigit
            loop rest (idx + (digits |> List.length)) symAcc ((parseDigits idx digits) :: numAcc)
        | head :: rest -> loop rest (idx + 1) (Array.append symAcc [| { T = head; X = idx; Y = lineIdx } |]) numAcc

    loop (line |> Seq.toList) 0 [||] []

let parseInput (lines: string list) : Number list * Symbol array =
    List.mapi (fun i s -> parseLine i s) lines
    |> List.reduce (fun (nums1, sym1) (nums2, sym2) -> (List.append nums1 nums2, Array.append sym1 sym2))

module Part1 =
    let hasAdjacentSymbol (num: Number) (symbols: Symbol array) : bool =
        let rangeX = [ num.StartX - 1 .. num.EndX + 1 ]
        let rangeY = [ num.Y - 1 .. num.Y + 1 ]

        symbols
        |> Array.exists (fun s -> List.contains s.Y rangeY && List.contains s.X rangeX)

    let compute input =
        let nums, symbols = parseInput input
        nums |> List.filter (fun n -> hasAdjacentSymbol n symbols) |> List.sumBy _.Num

    let run input =
        let result = compute input
        printfn $"Part1: {result}"

module Part2 =
    let withAdjacentSymbol (num: Number) (symbols: Symbol array) : (Number * Symbol) option =
        let rangeX = [ num.StartX - 1 .. num.EndX + 1 ]
        let rangeY = [ num.Y - 1 .. num.Y + 1 ]

        symbols
        |> Array.tryFind (fun s -> List.contains s.Y rangeY && List.contains s.X rangeX)
        |> function
            | Some symbol -> Some(num, symbol)
            | None -> None

    let multiplyNums n1 n2 = n1.Num * n2.Num

    let compute input =
        let nums, symbols = parseInput input
        let symbols = symbols |> Array.filter (fun s -> s.T = '*')

        nums
        |> List.choose (fun n -> withAdjacentSymbol n symbols)
        |> List.groupBy snd
        |> List.filter (fun (_, nums) -> (nums |> List.length = 2))
        |> List.map (snd >> List.map fst)
        |> List.map (fun ns -> multiplyNums (List.head ns) (List.item 1 ns))
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2 {result}"

let run (part: Parts) =
    let input = readLines "day03-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
