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
