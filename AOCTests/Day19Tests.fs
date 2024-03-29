﻿module AOCTests.Day19Tests

open Xunit
open AOC2023.Day19
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Theory>]
[<InlineData("day19-example.txt", 19114)>]
[<InlineData("day19-input.txt", 386787)>]
let ``part1 computes correctly`` (fileName: string) (expected: int) =
    let input = readText fileName
    let result = Part1.compute input
    test <@ result = expected @>

[<Theory>]
[<InlineData("day19-example.txt", 167409079868000L)>]
[<InlineData("day19-input.txt", 131029523269531L)>]
let ``part2 computes correctly`` (fileName: string) (expected: int64) =
    let input = readText fileName
    let result = Part2.compute input
    test <@ result = expected @>
