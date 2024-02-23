module AOCTests.Day16Tests

open Xunit
open AOC2023.Day16
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day16-example.txt", 46)>]
[<InlineData("day16-input.txt", 6906)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day16-example.txt", 51)>]
[<InlineData("day16-input.txt", 7330)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
