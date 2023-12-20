module AOCTests.Day05Tests

open Xunit
open AOC2023.Day05
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day05-example.txt", 35)>]
[<InlineData("day05-input.txt", 579439039)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let data = readText input
    let result = Part1.compute data
    test <@ result = expected @>

[<Theory>]
[<InlineData("day05-example.txt", 0)>]
// [<InlineData("day05-input.txt", 0)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
