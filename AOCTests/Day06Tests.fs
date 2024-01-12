module AOCTests.Day06Tests

open Xunit
open AOC2023.Day06
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day06-example.txt", 288)>]
[<InlineData("day06-input.txt", 160816)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day06-example.txt", 71503L)>]
[<InlineData("day06-input.txt", 46561107L)>]
let ``part2 computes correctly`` (input: string) (expected: int64) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
