open System
open System.Text
open System.IO


let areAllUnique input =
    let set = input |> Set.ofArray
    set.Count = input.Length


let getStarterPackagePosition (windowed: list<array<char>>) =
    let rec rf count (windowed: list<array<char>>) =
        let h = windowed.Head
        let t = windowed.Tail

        match windowed with
        | [] -> -1
        | head :: tail ->
            if areAllUnique head then
                count + head.Length
            else
                rf (count + 1) tail

    rf 0 windowed


[<EntryPoint>]
let main argv =

    let input = File.ReadAllText("/home/emile/programming/aoc22/src/aoc6/input")
    let windowed = input |> Seq.windowed 14 |> Seq.toList
    printfn "start position %i" (getStarterPackagePosition windowed)
    0
