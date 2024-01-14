module AOCTests.Day09Tests

open Xunit
open AOC2023.Day09
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day09-example.txt", 114)>]
[<InlineData("day09-input.txt", 1904165718)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day09-example.txt", 0)>]
// [<InlineData("day09-input.txt", 0)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
