﻿module AOCTests.Day17Tests

open Xunit
open AOC2023.Day17
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day17-example.txt", 102)>]
[<InlineData("day17-input.txt", 722)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day17-example.txt", 0)>]
// [<InlineData("day17-input.txt", 0)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
