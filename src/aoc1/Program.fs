open System
open System.IO


let lines = File.ReadAllLines(@"/home/emile/recreational_programming/aoc22/src/aoc1/input")

// fold the array into a list<list<string>>, where each list<string> correspondes to an elve
let elves =
    ([[]], lines)
    ||> Array.fold (fun (acc: list<list<string>>) (line: string) ->
                    match line.Length with
                    | 0 -> [] :: acc
                    | _ -> (line :: acc.Head) :: acc)

// parse the strings to ints
let calories = elves |> List.map (fun l -> l |> List.map (fun e -> Int32.Parse(e)))

// sum to get the total calories of each elve
let maxcalories = calories |> List.map (fun l -> l |> List.sum)


[<EntryPoint>]
let main argv =
    printfn "max: %i" (maxcalories |> List.max)
    printfn "sum of top 3: %i" (maxcalories |> List.sortDescending |> fun l -> l[..2] |> List.sum)
    0
