module AOCTests.TestsHelpers

// This is a hack, copy of the main CommonIO module to load input files because tests have different running
// directories.

open System
open System.IO

let inputPath file =
    [| Environment.CurrentDirectory; ".."; ".."; ".."; ".."; "inputs"; file |]
    |> Path.Combine

let readLines file = File.ReadLines(inputPath file)
let readText file = File.ReadAllText(inputPath file)
