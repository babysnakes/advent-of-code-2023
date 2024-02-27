module AOCTests.Day18Tests

open Xunit
open AOC2023.Day18
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day18-example.txt", 62)>]
[<InlineData("day18-input.txt", 39194)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day18-example.txt", 952408144115L)>]
[<InlineData("day18-input.txt", 78242031808225L)>]
let ``part2 computes correctly`` (input: string) (expected: int64) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
