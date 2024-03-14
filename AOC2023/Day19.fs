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
type Workflow = { Rules: Rule list; DefaultTarget: Target }

let mkRule part op n target =
    { Part = part; Operator = op; Amount = n; Target = target }

let mkWorkflow rules target =
    { Rules = rules; DefaultTarget = target }


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

let parseWorkflow = run pWorkflow >> unwrap

module Part1 =
    type PartsRating = { X: int; M: int; A: int; S: int }

    let ratingSum r = r.X + r.M + r.A + r.S
    let mkRating x m a s = { X = x; M = m; A = a; S = s }

    let pRating =
        pipe4
            (str "{x=" >>. pint32)
            (str ",m=" >>. pint32)
            (str ",a=" >>. pint32)
            (str ",s=" >>. pint32 .>> pchar '}')
            mkRating

    let parseRating = run pRating >> unwrap

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
    type PartRange = { Min: int; Max: int }

    type PartsRatingsRanges = { X: PartRange; M: PartRange; A: PartRange; S: PartRange }

    let fullRange =
        { X = { Min = 1; Max = 4000 }
          M = { Min = 1; Max = 4000 }
          A = { Min = 1; Max = 4000 }
          S = { Min = 1; Max = 4000 } }

    let splitRange (range: PartsRatingsRanges) (rule: Rule) : PartsRatingsRanges * PartsRatingsRanges =
        let split (r: PartRange) =
            match rule.Operator, r with
            | Less, { Max = n } when n < rule.Amount -> (r, { Min = rule.Amount; Max = rule.Amount })
            | Less, { Min = n } when n >= rule.Amount -> ({ r with Max = r.Min }, r)
            | Less, r -> ({ r with Max = rule.Amount - 1 }, { r with Min = rule.Amount })
            | More, { Min = n } when n > rule.Amount -> (r, { Min = rule.Amount; Max = rule.Amount })
            | More, { Max = n } when n <= rule.Amount -> ({ r with Min = r.Max }, r)
            | More, r -> ({ r with Min = rule.Amount + 1 }, { r with Max = rule.Amount })

        match rule.Part with
        | X ->
            let accept, reject = split range.X
            { range with X = accept }, { range with X = reject }
        | M ->
            let accept, reject = split range.M
            { range with M = accept }, { range with M = reject }
        | A ->
            let accept, reject = split range.A
            { range with A = accept }, { range with A = reject }
        | S ->
            let accept, reject = split range.S
            { range with S = accept }, { range with S = reject }

    let findAcceptedRanges map =
        let rec loop queue acc =
            match queue with
            | [] -> acc
            | (range, workflow) :: rest ->
                match workflow with
                | { Rules = []; DefaultTarget = target } ->
                    match target with
                    | Accept -> loop rest (range :: acc)
                    | Reject -> loop rest acc
                    | Name s ->
                        let wf = Map.find s map
                        loop ((range, wf) :: rest) acc
                | { Rules = r :: rs } ->
                    let this, others = splitRange range r

                    match r.Target with
                    | Accept -> loop ((others, { workflow with Rules = rs }) :: rest) (this :: acc)
                    | Reject -> loop ((others, { workflow with Rules = rs }) :: rest) acc
                    | Name s ->
                        let this, others = splitRange range r
                        let targetWf = Map.find s map
                        loop ((this, targetWf) :: (others, { workflow with Rules = rs }) :: rest) acc

        let startingWf = Map.find "in" map
        loop [ (fullRange, startingWf) ] List.empty

    let countSolutionsInRange (r: PartsRatingsRanges) =
        [ r.X.Max - r.X.Min + 1 |> int64
          r.M.Max - r.M.Min + 1 |> int64
          r.A.Max - r.A.Min + 1 |> int64
          r.S.Max - r.S.Min + 1 |> int64 ]
        |> List.reduce (*)

    let compute (input: string) =
        let nl = Environment.NewLine
        let parts = input.Split($"{nl}{nl}")

        parts[0].Split($"{nl}")
        |> List.ofArray
        |> List.map parseWorkflow
        |> Map
        |> findAcceptedRanges
        |> List.map countSolutionsInRange
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readText "day19-input.txt"

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
