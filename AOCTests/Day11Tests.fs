module AOCTests.Day11Tests

open Xunit
open AOC2023.Day11
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day11-example.txt", 374)>]
[<InlineData("day11-input.txt", 9177603)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day11-example.txt", 82000210L)>]
[<InlineData("day11-input.txt", 632003913611L)>]
let ``part2 computes correctly`` (input: string) (expected: int64) =
    let lines = readLines input
    let result = Part2.compute lines
    test <@ result = expected @>
