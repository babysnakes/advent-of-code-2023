namespace AOC2023

module CommonIO =
    open System.IO

    let getInputPath fileName =
        [| "inputs"; fileName |] |> Path.Combine

    let readLines file = File.ReadLines(getInputPath file)
    let readText file = File.ReadAllText(getInputPath file)


module CommonString =
    let split (sep: string) (s: string) = s.Split(sep)


module Parser =
    open FParsec

    let unwrap<'T> (res: ParserResult<'T, unit>) : 'T =
        match res with
        | Success(value, _, _) -> value
        | Failure(error, _, _) -> failwith $"parser error: {error}"

module Collections =

    // https://stackoverflow.com/questions/49888944/f-find-index-of-element-in-2d-array
    [<RequireQualifiedAccess>]
    module Array2D =
        let find2D needle (arr: 'T[,]) =
            let rec go x y =
                if y >= arr.GetLength 1 then None
                elif x >= arr.GetLength 0 then go 0 (y + 1)
                elif arr.[x, y] = needle then Some(x, y)
                else go (x + 1) y

            go 0 0
