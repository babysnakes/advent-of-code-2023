module AOCTests.Day02Tests

open Xunit
open AOC2023.Day02
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day02-example.txt", 8)>]
[<InlineData("day02-input.txt", 2795)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day02-example.txt", 2286)>]
[<InlineData("day02-input.txt", 75561)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
