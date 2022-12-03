open System
open System.IO
open System.Text


let lines = File.ReadLines("/home/emile/recreational_programming/aoc22/src/aoc3/input")

let rucksacks : array<array<byte>> =
    lines
    |> Seq.map (fun s -> Encoding.ASCII.GetBytes(s))
    |> Seq.toArray

let convertToValue (b:byte) =
    let res = ((int b)) - 96
    if res > 0 then
        res
    else
        res + 58

let splitRucksackIntoCompartements (rucksack: array<byte>) =
    let l = rucksack.Length / 2
    rucksack[..l-1], rucksack[l..]

let findCommonItem (c1: array<byte>, c2: array<byte>) =
    let s1 = Set.ofArray c1
    let s2 = Set.ofArray c2

    Set.intersect s1 s2 |> Set.toArray |> fun a -> a[0]
    
let findCommonItem3(group: byte array[]):byte =
    let sets = group |> Array.map (fun a -> a |> Set.ofArray) 
    Set.intersectMany sets |> Set.toArray |> fun a -> a[0]

    
[<EntryPoint>]
let main argv =
    let sum = (0, rucksacks) ||> Array.fold (fun acc r ->
                                             (r
                                             |> splitRucksackIntoCompartements
                                             |> findCommonItem
                                             |> convertToValue)
                                             + acc)
      
    let groups = Array.chunkBySize 3 rucksacks

    let commonSum = (0, groups) ||> Array.fold (fun acc g ->
                                                acc  + (g
                                                        |> findCommonItem3
                                                        |> convertToValue))
                                                
    printfn "Sum is %i" sum
    printfn "Common sum is %i" commonSum
    0
