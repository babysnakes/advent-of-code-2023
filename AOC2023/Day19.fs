module AOC2023.Day19

open System
open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser

type Part =
    | X
    | M
    | A
    | S

type Target =
    | Accept
    | Reject
    | Name of string

type Op =
    | Less
    | More

type Rule = { Part: Part; Operator: Op; Amount: int; Target: Target }
type PartsRating = { X: int; M: int; A: int; S: int }
type Workflow = { Rules: Rule list; DefaultTarget: Target }

let mkRule part op n target =
    { Part = part; Operator = op; Amount = n; Target = target }

let mkWorkflow rules target =
    { Rules = rules; DefaultTarget = target }

let mkRating x m a s = { X = x; M = m; A = a; S = s }

let str s = pstring s
let pIdent = regex "[a-z]+"
let partA = pchar 'a' >>% A
let partX = pchar 'x' >>% X
let partM = pchar 'm' >>% M
let partS = pchar 's' >>% S
let pPart = partA <|> partX <|> partM <|> partS
let pLess = pchar '<' >>% Less
let pMore = pchar '>' >>% More
let pOp = pLess <|> pMore
let pAccept = pchar 'A' >>% Accept
let pReject = pchar 'R' >>% Reject
let pName = pIdent |>> Name
let pTarget = pAccept <|> pReject <|> pName
let pCondition = pipe4 pPart pOp pint32 (pchar ':' >>. pTarget .>> pchar ',') mkRule
let pRules = pipe2 (many (attempt pCondition)) pTarget mkWorkflow
let pWorkflow = pIdent .>> pchar '{' .>>. pRules .>> pchar '}'

let pRating =
    pipe4
        (str "{x=" >>. pint32)
        (str ",m=" >>. pint32)
        (str ",a=" >>. pint32)
        (str ",s=" >>. pint32 .>> pchar '}')
        mkRating

let parseWorkflow = run pWorkflow >> unwrap
let parseRating = run pRating >> unwrap

module Part1 =
    let ratingSum r = r.X + r.M + r.A + r.S

    let checkRule rule rating =
        match (rule.Part, rule.Operator) with
        | X, Less -> rating.X < rule.Amount
        | X, More -> rating.X > rule.Amount
        | M, Less -> rating.M < rule.Amount
        | M, More -> rating.M > rule.Amount
        | A, Less -> rating.A < rule.Amount
        | A, More -> rating.A > rule.Amount
        | S, Less -> rating.S < rule.Amount
        | S, More -> rating.S > rule.Amount

    let rec runWorkflow workflow rating map =
        match workflow.Rules with
        | [] ->
            match workflow.DefaultTarget with
            | Accept -> Some rating
            | Reject -> None
            | Name ident -> runWorkflow (Map.find ident map) rating map
        | rule :: rules ->
            if checkRule rule rating then
                match rule.Target with
                | Accept -> Some rating
                | Reject -> None
                | Name ident -> runWorkflow (Map.find ident map) rating map
            else
                runWorkflow { workflow with Rules = rules } rating map

    let compute (input: string) =
        let nl = Environment.NewLine
        let parts = input.Split($"{nl}{nl}")

        let rulesMap =
            parts[0].Split($"{nl}") |> List.ofArray |> List.map parseWorkflow |> Map

        let inWorkflow = rulesMap |> Map.find "in"

        parts[1].Split($"{nl}")
        |> List.ofArray
        |> List.map parseRating
        |> List.choose (fun r -> runWorkflow inWorkflow r rulesMap)
        |> List.map ratingSum
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =

    let compute (input: string) = 0

    let run input =
        let result = compute input
        printfn $"Part2: {result}"
        printfn "NOT IMPLEMENTED"


let run (part: Parts) =
    let input = readText "day19-input.txt"

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
