module AOC2023.Day01

open System
open AOC2023.Models
open AOC2023.CommonIO

module Part1 =
    let getCalibrationValue (line: string) =
        let chars = Seq.toArray line
        let first = Seq.find Char.IsDigit chars
        let last = Seq.findBack Char.IsDigit chars
        $"{first}{last}" |> int

    let compute lines =
        lines |> Seq.map getCalibrationValue |> Seq.sum

    let run input =
        let result = input |> compute
        printfn $"Part 1: {result}"

let run (part: Parts) =
    let input = readLines "day01-input.txt"
    Part1.run input
