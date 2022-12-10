open System
open System.IO
open System.Text

type Coord = { x: int; y: int }

let countVisible (grid: list<list<int>>) cols (cords: Set<Coord>) row =
    let res =
        ({| set = cords; highestSeen = -1 |}, cols)
        ||> List.fold (fun acc col ->
            Console.WriteLine($"x={col};y={row}\t{grid[row][col]}\t{acc.highestSeen}")
            if grid[row][col] > acc.highestSeen then
                {| acc with
                    highestSeen  = grid[row][col]
                    set = Set.add { x = row; y = col } acc.set |}
            else
                acc)

    res.set

[<EntryPoint>]
let main arv =
    let input = File.ReadAllLines("/home/emile/programming/aoc22/src/aoc8/test_input")

    let grid =
        input
        |> Array.toList
        |> List.map (fun r -> r |> Seq.map (fun s -> int s - int '0') |> Seq.toList)

    let rows = [ 0 .. grid.Length-1 ]
    let cols = [ 0 .. grid[0].Length-1 ]

    let res =
        (Set.empty, rows)
        ||> List.fold (fun acc row -> countVisible grid cols acc row)
        |> (fun s ->
            Console.WriteLine("====")
            (s, cols)
            ||> List.fold (fun acc col -> countVisible grid rows acc col))
        |> (fun s ->
            Console.WriteLine("====")
            (s, List.rev rows)
            ||> List.fold (fun acc row -> countVisible grid cols acc row))
        |> (fun s ->
            Console.WriteLine("====")
            (s, List.rev cols)
            ||> List.fold (fun acc col -> countVisible grid rows acc col))

    for i in res do
        Console.WriteLine($"{i.x} {i.y}")
    printfn "%i" res.Count
    0
