module AOCTests.Day07Tests

open Xunit
open AOC2023.Day07
open AOCTests.TestsHelpers
open Swensen.Unquote.Assertions

let parseCardsSamples: obj[] list =
    [ [| [ A; J; Two; Two; Three ]; OnePair [ A; J; Two; Two; Three ] |]
      [| [ Q; Three; Q; Q; Q ]; FourOfKind [ Q; Three; Q; Q; Q ] |]
      [| [ J; J; J; J; J ]; FiveOfKind [ J; J; J; J; J ] |]
      [| [ Four; Six; Q; Six; Four ]; TwoPairs [ Four; Six; Q; Six; Four ] |]
      [| [ Four; J; Four; Four; A ]; ThreeOfKind [ Four; J; Four; Four; A ] |]
      [| [ Three; A; J; Four; Ten ]; HighCard [ Three; A; J; Four; Ten ] |]
      [| [ Three; Three; J; Three; J ]; FullHouse [ Three; Three; J; Three; J ] |] ]

[<Theory>]
[<MemberData(nameof (parseCardsSamples))>]
let ``parseCards correctly parses hand`` (hand: Cards, expected: Hand) =
    let result = parseCards hand
    test <@ result = expected @>

[<Theory>]
[<InlineData("day07-example.txt", 6440)>]
[<InlineData("day07-input.txt", 247961593)>]
let ``part1 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part1.compute lines
    test <@ result = expected @>

[<Theory>]
[<InlineData("day07-example.txt", 0)>]
// [<InlineData("day07-input.txt", 0)>]
let ``part2 computes correctly`` (input: string) (expected: int) =
    let lines = readLines input |> List.ofSeq
    let result = Part2.compute lines
    test <@ result = expected @>
