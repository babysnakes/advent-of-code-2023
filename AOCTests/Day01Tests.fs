module AOCTests.Day01Tests

open Xunit
open AOC2023.Day01
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

[<Fact>]
let ``part1 computes correctly`` () =
    let expected = 56506
    let input = readLines "day01-input.txt"
    let result = Part1.compute input
    test <@ result = expected @>

[<Fact>]
let ``part2 computes correctly`` () =
    let expected = 56017
    let input = readLines "day01-input.txt"
    let result = Part2.compute input
    test <@ result = expected @>

module Helpers =
    [<Theory>]
    [<InlineData("eightwothree", "823")>]
    [<InlineData("xtwone3four", "2134")>]
    [<InlineData("4nineeightseven2", "49872")>]
    [<InlineData("twotsszxmtmvvdldqcdkvbtd7one", "271")>]
    let ``preparse line works correctly`` (s: string) (expected: string) =
        let result = s |> Seq.toList |> Part2.preparse
        test <@ result = (expected |> Seq.toList) @>
