module AOCTests.Day10Tests

open Xunit
open AOC2023.Day10
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day10-example1.txt", 4)>]
[<InlineData("day10-example2.txt", 8)>]
[<InlineData("day10-input.txt", 6838)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day10-example3.txt", 4)>]
[<InlineData("day10-example4.txt", 8)>]
[<InlineData("day10-example5.txt", 10)>]
[<InlineData("day10-input.txt", 451)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
