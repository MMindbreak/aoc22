open System
open System.IO


let text = File.ReadAllText(@"/home/emile/recreational_programming/aoc22/src/aoc1/input")

let lines = text.Split("\n")


let elves =
    ([[]], lines)
    ||> Array.fold (fun (acc: list<list<string>>) (line: string) ->
                    match line.Length with
                    | 0 -> [] :: acc
                    | _ -> (line :: acc.Head) :: acc)

let calories = elves |> List.map (fun l -> l |> List.map (fun e -> Int32.Parse(e)))

let maxcalories = calories |> List.map (fun l -> l |> List.sum)


[<EntryPoint>]
let main argv =
    printfn "max: %i" (maxcalories |> List.max)
    0
