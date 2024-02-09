module AOCTests.Day14Tests

open Xunit
open AOC2023.Day14
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day14-example.txt", 136)>]
[<InlineData("day14-input.txt", 102497)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> Seq.toArray
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day14-example.txt", 0)>]
// [<InlineData("day14-input.txt", 0)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
