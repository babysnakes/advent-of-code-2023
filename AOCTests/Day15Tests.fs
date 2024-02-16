module AOCTests.Day15Tests

open Xunit
open AOC2023.Day15
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day15-example.txt", 1320)>]
[<InlineData("day15-input.txt", 513172)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let input = readLines input |> List.ofSeq |> List.head
    let result = Part1.compute input
    test <@ result = expected @>

[<Theory>]
[<InlineData("day15-example.txt", 145)>]
[<InlineData("day15-input.txt", 237806)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let input = readLines input |> List.ofSeq |> List.head
    let result = Part2.compute input
    test <@ result = expected @>
