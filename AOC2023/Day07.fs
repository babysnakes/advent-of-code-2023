﻿module AOC2023.Day07

open FParsec
open AOC2023.Models
open AOC2023.CommonIO
open AOC2023.Parser

module Part1 =
    type Card =
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | J
        | Q
        | K
        | A

    type Cards = Card list

    type Hand =
        | HighCard of Cards
        | OnePair of Cards
        | TwoPairs of Cards
        | ThreeOfKind of Cards
        | FullHouse of Cards
        | FourOfKind of Cards
        | FiveOfKind of Cards

    let parseCards cards =
        match cards |> List.countBy id |> List.sortByDescending snd with
        | (_, 5) :: _ -> FiveOfKind cards
        | (_, 4) :: _ -> FourOfKind cards
        | (_, 3) :: (_, 2) :: _ -> FullHouse cards
        | (_, 3) :: _ -> ThreeOfKind cards
        | (_, 2) :: (_, 2) :: _ -> TwoPairs cards
        | (_, 2) :: _ -> OnePair cards
        | _ -> HighCard cards

    let parseHand (cards, bid) =
        let hand = parseCards cards
        (hand, bid)

    let pTwo = pchar '2' >>% Two
    let pThree = pchar '3' >>% Three
    let pFour = pchar '4' >>% Four
    let pFive = pchar '5' >>% Five
    let pSix = pchar '6' >>% Six
    let pSeven = pchar '7' >>% Seven
    let pEight = pchar '8' >>% Eight
    let pNine = pchar '9' >>% Nine
    let pTen = pchar 'T' >>% Ten
    let pJack = pchar 'J' >>% J
    let pQueen = pchar 'Q' >>% Q
    let pKing = pchar 'K' >>% K
    let pAce = pchar 'A' >>% A

    let pCard =
        pTwo
        <|> pThree
        <|> pFour
        <|> pFive
        <|> pSix
        <|> pSeven
        <|> pEight
        <|> pNine
        <|> pTen
        <|> pJack
        <|> pQueen
        <|> pKing
        <|> pAce

    let pCards = many pCard
    let pLine = pCards .>> spaces .>>. pint32 |>> parseHand


    let compute input =
        input
        |> List.map (run pLine >> unwrap)
        |> List.sortBy fst
        |> List.mapi (fun idx (_, bid) -> bid * (idx + 1))
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part1: {result}"


module Part2 =
    type Card =
        | J
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | Q
        | K
        | A

    type Cards = Card list

    type Hand =
        | HighCard of Cards
        | OnePair of Cards
        | TwoPairs of Cards
        | ThreeOfKind of Cards
        | FullHouse of Cards
        | FourOfKind of Cards
        | FiveOfKind of Cards

    let parseCards cards =
        let partition = cards |> List.countBy id |> List.sortByDescending snd

        let jokers =
            partition
            |> List.tryFind (fun (c, _) -> c = J)
            |> Option.map snd
            |> Option.defaultValue 0

        match jokers, partition with
        | _, (_, 5) :: _ -> FiveOfKind cards
        | 0, (_, 4) :: _ -> FourOfKind cards
        | _, (_, 4) :: _ -> FiveOfKind cards
        | _, (J, 3) :: (_, 2) :: _ -> FiveOfKind cards
        | _, (_, 3) :: (J, 2) :: _ -> FiveOfKind cards
        | _, (_, 3) :: (_, 2) :: _ -> FullHouse cards
        | _, (J, 3) :: _ -> FourOfKind cards
        | 1, (_, 3) :: _ -> FourOfKind cards
        | _, (_, 3) :: _ -> ThreeOfKind cards
        | 2, (_, 2) :: (_, 2) :: _ -> FourOfKind cards
        | 1, (_, 2) :: (_, 2) :: _ -> FullHouse cards
        | _, (_, 2) :: (_, 2) :: _ -> TwoPairs cards
        | 1, (_, 2) :: _ -> ThreeOfKind cards
        | _, (J, 2) :: _ -> ThreeOfKind cards
        | _, (_, 2) :: _ -> OnePair cards
        | 1, _ -> OnePair cards
        | _, _ -> HighCard cards

    let parseHand (cards, bid) =
        let hand = parseCards cards
        (hand, bid)

    let pTwo = pchar '2' >>% Two
    let pThree = pchar '3' >>% Three
    let pFour = pchar '4' >>% Four
    let pFive = pchar '5' >>% Five
    let pSix = pchar '6' >>% Six
    let pSeven = pchar '7' >>% Seven
    let pEight = pchar '8' >>% Eight
    let pNine = pchar '9' >>% Nine
    let pTen = pchar 'T' >>% Ten
    let pJoker = pchar 'J' >>% J
    let pQueen = pchar 'Q' >>% Q
    let pKing = pchar 'K' >>% K
    let pAce = pchar 'A' >>% A

    let pCard =
        pTwo
        <|> pThree
        <|> pFour
        <|> pFive
        <|> pSix
        <|> pSeven
        <|> pEight
        <|> pNine
        <|> pTen
        <|> pJoker
        <|> pQueen
        <|> pKing
        <|> pAce

    let pCards = many pCard
    let pLine = pCards .>> spaces .>>. pint32 |>> parseHand


    let compute input =
        input
        |> List.map (run pLine >> unwrap)
        |> List.sortBy fst
        |> List.mapi (fun idx (_, bid) -> bid * (idx + 1))
        |> List.sum

    let run input =
        let result = compute input
        printfn $"Part2: {result}"


let run (part: Parts) =
    let input = readLines "day07-input.txt" |> Seq.toList

    match part with
    | Part1 -> Part1.run input
    | Part2 -> Part2.run input
    | Unspecified ->
        Part1.run input
        Part2.run input
