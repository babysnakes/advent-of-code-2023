module AOCTests.Day08Tests

open Xunit
open AOC2023.Day08
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day08-example1.txt", 6)>]
[<InlineData("day08-input.txt", 17873)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day08-example2.txt", 6)>]
[<InlineData("day08-input.txt", 15746133679061L)>]
let ``part2 computes correctly`` (input: string) (expected: int64) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
