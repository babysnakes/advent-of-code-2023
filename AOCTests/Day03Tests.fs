module AOCTests.Day03Tests

open Xunit
open AOC2023.Day03
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day03-example.txt", 4361)>]
[<InlineData("day03-input.txt", 544664)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>
