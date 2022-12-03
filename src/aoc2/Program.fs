open System
open System.IO
open FSharp.Collections

let lines =
    File.ReadLines("/home/emile/recreational_programming/aoc22/src/aoc2/input")



type Hand =
    | Rock
    | Paper
    | Scissors

let getHandValue hand =
    match hand with
    | Rock -> 1
    | Scissors -> 3
    | Paper -> 2

type Outcome =
    | Loss
    | Equal
    | Win

let getOutcomeValue outcome =
    match outcome with
    | Loss -> 0
    | Equal -> 3
    | Win -> 6

let matchEnemy input : Hand =
    match input with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> raise (ArgumentException($"{input} is not a valid value"))

let matchRecommendation input =
    match input with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> raise (ArgumentException($"{input} is not a valid value"))


let matchDesiredOutcome input =
    match input with
    | "X" -> Loss
    | "Y" -> Equal
    | "Z" -> Win
    | _ -> raise (ArgumentException($"{input} is not a valid value"))

let readLine (line: string) =
    let inputs = line.Split(" ")
    matchEnemy (inputs[0]), matchRecommendation (inputs[1])

let readLineDesiredOutcome (line: string) =
    let inputs = line.Split(" ")
    matchEnemy inputs[0], matchDesiredOutcome inputs[1]

let getHandToPlay (opponent: Hand) (desiredOutcome: Outcome) =
    match opponent, desiredOutcome with
    | Rock, Win -> Paper
    | Rock, Equal -> Rock
    | Rock, Loss -> Scissors
    | Paper, Win -> Scissors
    | Paper, Equal -> Paper
    | Paper, Loss -> Rock
    | Scissors, Win -> Rock
    | Scissors, Equal -> Scissors
    | Scissors, Loss -> Paper

let getOutcome (opponent: Hand) (player: Hand) =
    match opponent, player with
    | Rock, Paper -> Win
    | Rock, Scissors -> Loss
    | Paper, Scissors -> Win
    | Paper, Rock -> Loss
    | Scissors, Rock -> Win
    | Scissors, Paper -> Loss
    | _ -> Equal

let getScore hand outcome =
    (getHandValue hand) + (getOutcomeValue outcome)

[<EntryPoint>]
let main argv =

    let endScore =
        (0, lines)
        ||> Seq.fold (fun acc line ->
            let (opponent, desiredOutcome) = readLineDesiredOutcome line
            let handToPlay = getHandToPlay opponent desiredOutcome
            let outcome = getOutcome opponent handToPlay
            acc + (getScore handToPlay outcome))

    printfn "Total Score: %i" endScore
    0
