module AOC2023.Day17

open System.Collections.Generic
open AOC2023.Models
open AOC2023.CommonIO

type Direction =
    | U
    | D
    | L
    | R

type Step =
    { Idx1: int
      Idx2: int
      Sid: int // steps in direction
      Sum: int
      Dir: Direction }

type State = { Walking: Step list; Done: Step list }
type PQ = PriorityQueue<Step, int>

let mkMap (input: string list) = array2D input

module Part1 =
    // The idea to use priority queue is from the great @hyper-neutrino youtube channel
    let pq = PQ()

    let calculateSteps map acc (cur: Step) : Step list =
        let maxIdx = (map |> Array2D.length1) - 1

        let checkStep =
            function
            | { Idx1 = idx1 } when idx1 < 0 || idx1 > maxIdx -> None
            | { Idx2 = idx2 } when idx2 < 0 || idx2 > maxIdx -> None
            | { Sid = n } when n > 3 -> None
            | { Idx1 = idx1; Idx2 = idx2; Dir = dir; Sid = sid } when Set.contains (idx1, idx2, dir, sid) acc -> None
            | step ->
                { step with
                    Sum = (step.Sum + int map[step.Idx1, step.Idx2]) }
                |> Some

        let goUp sid =
            checkStep { cur with Idx1 = cur.Idx1 - 1; Dir = U; Sid = sid }

        let goDown sid =
            checkStep { cur with Idx1 = cur.Idx1 + 1; Dir = D; Sid = sid }

        let goLeft sid =
            checkStep { cur with Idx2 = cur.Idx2 - 1; Dir = L; Sid = sid }

        let goRight sid =
            checkStep { cur with Idx2 = cur.Idx2 + 1; Dir = R; Sid = sid }

        match cur.Dir with
        | U -> [ goUp (cur.Sid + 1); goLeft 1; goRight 1 ]
        | D -> [ goDown (cur.Sid + 1); goRight 1; goLeft 1 ]
        | L -> [ goLeft (cur.Sid + 1); goUp 1; goDown 1 ]
        | R -> [ goRight (cur.Sid + 1); goUp 1; goDown 1 ]
        |> List.choose id

    let bestPath map =
        let maxIdx = (map |> Array2D.length1) - 1
        let entry = { Idx1 = 0; Idx2 = 0; Sid = 0; Sum = 0; Dir = R }

        let rec loop acc =
            let processStep = calculateSteps map acc
            let step = pq.Dequeue()

            if step.Idx1 = maxIdx && step.Idx2 = maxIdx then
                step.Sum
            else
                let nextSteps = processStep step
                nextSteps |> List.iter (fun s -> pq.Enqueue(s, s.Sum))

                nextSteps
                |> List.map (fun step -> (step.Idx1, step.Idx2, step.Dir, step.Sid))
                |> Set.ofList
                |> Set.union acc
                |> loop

        pq.Enqueue(entry, 0)
        loop ([ (0, 0, R, 0) ] |> Set.ofList)

    let compute input =
        input |> mkMap |> Array2D.map string |> bestPath

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    let pq = PQ()

    let calculateSteps map acc (cur: Step) : Step list =
        let maxIdx1 = (map |> Array2D.length1) - 1
        let maxIdx2 = (map |> Array2D.length2) - 1

        let checkStep =
            function
            | { Idx1 = idx1 } when idx1 < 0 || idx1 > maxIdx1 -> None
            | { Idx2 = idx2 } when idx2 < 0 || idx2 > maxIdx2 -> None
            | { Sid = n } when n > 10 -> None
            | { Dir = dir } when dir <> cur.Dir && cur.Sid < 4 -> None
            | { Idx1 = idx1; Idx2 = idx2; Dir = dir; Sid = sid } when Set.contains (idx1, idx2, dir, sid) acc -> None
            | step ->
                { step with
                    Sum = (step.Sum + int map[step.Idx1, step.Idx2]) }
                |> Some

        let goUp sid =
            checkStep { cur with Idx1 = cur.Idx1 - 1; Dir = U; Sid = sid }

        let goDown sid =
            checkStep { cur with Idx1 = cur.Idx1 + 1; Dir = D; Sid = sid }

        let goLeft sid =
            checkStep { cur with Idx2 = cur.Idx2 - 1; Dir = L; Sid = sid }

        let goRight sid =
            checkStep { cur with Idx2 = cur.Idx2 + 1; Dir = R; Sid = sid }

        match cur.Dir with
        | U -> [ goUp (cur.Sid + 1); goLeft 1; goRight 1 ]
        | D -> [ goDown (cur.Sid + 1); goRight 1; goLeft 1 ]
        | L -> [ goLeft (cur.Sid + 1); goUp 1; goDown 1 ]
        | R -> [ goRight (cur.Sid + 1); goUp 1; goDown 1 ]
        |> List.choose id

    let bestPath map =
        let maxIdx1 = (map |> Array2D.length1) - 1
        let maxIdx2 = (map |> Array2D.length2) - 1
        let entry1 = { Idx1 = 0; Idx2 = 0; Sid = 0; Sum = 0; Dir = R }
        let entry2 = { Idx1 = 0; Idx2 = 0; Sid = 0; Sum = 0; Dir = D }

        let rec loop acc =
            let processStep = calculateSteps map acc
            let step = pq.Dequeue()

            if step.Idx1 = maxIdx1 && step.Idx2 = maxIdx2 && step.Sid >= 4 then
                step.Sum
            else
                let nextSteps = processStep step
                nextSteps |> List.iter (fun s -> pq.Enqueue(s, s.Sum))

                nextSteps
                |> List.map (fun step -> (step.Idx1, step.Idx2, step.Dir, step.Sid))
                |> Set.ofList
                |> Set.union acc
                |> loop

        pq.Enqueue(entry1, 0)
        pq.Enqueue(entry2, 0)
        loop Set.empty


    let compute input =
        input |> mkMap |> Array2D.map string |> bestPath

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day17-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
