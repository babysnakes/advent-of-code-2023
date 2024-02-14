module AOCTests.Day15Tests

open Xunit
open AOC2023.Day15
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day15-example.txt", 1320L)>]
[<InlineData("day15-input.txt", 513172L)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let input = readLines input |> List.ofSeq |> List.head
    let result = Part1.compute input
    test <@ result = expected @>

[<Theory>]
[<InlineData("day15-example.txt", 0)>]
// [<InlineData("day15-input.txt", 0)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
