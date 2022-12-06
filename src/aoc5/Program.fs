open System
open System.Text
open System.IO

let text = File.ReadAllText("/home/emile/programming/aoc22/src/aoc5/test_input")

let split = text.Split("\n\n")

let crates = split[0]

let procedures = split[1]

type Stack = list<string> * int

let buildStacks (crates: string) =
    crates.Split("\n")
    |> fun a -> a[..a.Length-2]
    |> Array.map (fun r -> Encoding.ASCII.GetBytes r |> Array.chunkBySize 4)

let rows = buildStacks crates

let stackCount = rows[rows.Length - 1].Length

let accArr =
    seq {
        for x in 1..stackCount do
            [||]
    }
    |> Seq.toArray

let stacks =
    (accArr, rows)
    ||> Array.fold (fun acc row ->
        (acc, row)
        ||> Array.map2 (fun a r ->
            if not (String.IsNullOrWhiteSpace(Encoding.ASCII.GetString(r))) then
                        Array.concat [ a; r ]
            else
                a))


let crateStacks = stacks |> Array.map (fun s -> Encoding.ASCII.GetString(s).Split(" "))

    
type Procedure = { Amount: int; From: int; To: int }

let parseProcedure (line: string) =
    let tokens = line.Split(" ")
    { Amount = int tokens[1]
      From = (int tokens[3]) - 1
      To = (int tokens[5]) - 1}


let moveCrate fromStack toStack (crateStacks: string[][]) =
    crateStacks[toStack] <- Array.concat [ [| crateStacks[fromStack][0] |] ;crateStacks[toStack]]
    crateStacks[fromStack] <- crateStacks[fromStack][1..]
    crateStacks


let moveCrates fromStack toStack amount (crateStacks: string[][]) =
    crateStacks[toStack] <- Array.concat [ crateStacks[fromStack][..amount - 1] ;crateStacks[toStack]]
    crateStacks[fromStack] <- crateStacks[fromStack][amount..]
    crateStacks
    
let rec applyProcedure (p: Procedure) (stacks: string[][]) =
    if p.Amount = 0 then
        stacks
    else
        applyProcedure {p with Amount = p.Amount - 1} (moveCrate p.From p.To stacks)


let printit (crateStacks: string[][]) =
    for r in crateStacks do
        for c in r do
            Console.Write(c)
        Console.Write("\n")


printit crateStacks
for proc in procedures.Split("\n") do
    let procedure = parseProcedure proc
    Console.WriteLine("====")
    moveCrates procedure.From procedure.To procedure.Amount crateStacks |> ignore
    printit crateStacks
Console.WriteLine("====")

for stack in crateStacks do
    printfn "%s" stack[0]
