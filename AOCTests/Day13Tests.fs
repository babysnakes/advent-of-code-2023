module AOCTests.Day13Tests

open Xunit
open AOC2023.Day13
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day13-example.txt", 405)>]
[<InlineData("day13-input.txt", 31265)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readText input
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day13-example.txt", 400)>]
[<InlineData("day13-input.txt", 39359)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readText input
    let result = Part2.compute lines
    test <@ result = expected @>
