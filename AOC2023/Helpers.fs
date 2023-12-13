﻿namespace AOC2023

module CommonIO =
    open System.IO

    let getInputPath fileName =
        [| "inputs"; fileName |] |> Path.Combine

    let readLines file = File.ReadLines(getInputPath file)
    let readText file = File.ReadAllText(getInputPath file)


module CommonString =
    let split (sep: string) (s: string) = s.Split(sep)
