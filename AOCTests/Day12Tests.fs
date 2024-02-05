module AOCTests.Day12Tests

open Xunit
open AOC2023.Day12
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day12-example.txt", 21)>]
[<InlineData("day12-input.txt", 7490)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day12-example.txt", 525152L)>]
[<InlineData("day12-input.txt", 65607131946466L)>]
let ``part2 computes correctly`` (input: string) (expected: int64) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
