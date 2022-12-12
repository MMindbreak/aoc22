open System
open System.IO

type Direction =
    | Overlapp
    | Up
    | Down
    | Left
    | Right
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft

type Motion = { amount: int; direction: Direction }

type Coord = { x: int; y: int }

type Rope = { head: Coord; tail: Coord }

let translate coord direction =
    match direction with
    | Overlapp -> coord
    | Up -> { coord with y = coord.y + 1 }
    | Down -> { coord with y = coord.y - 1 }
    | Left -> { coord with x = coord.x - 1 }
    | Right -> { coord with x = coord.x + 1 }
    | UpRight ->
        { coord with
            x = coord.x + 1
            y = coord.y + 1 }
    | UpLeft ->
        { coord with
            x = coord.x - 1
            y = coord.y + 1 }
    | DownRight ->
        { coord with
            x = coord.x + 1
            y = coord.y - 1 }
    | DownLeft ->
        { coord with
            x = coord.x - 1
            y = coord.y - 1 }

let distance (x: Coord) (y: Coord) =
    List.max [ Math.Abs(x.x - y.x); Math.Abs(x.y - y.y) ]

let orientation (head: Coord) (tail: Coord) =
    let hx = head.x
    let hy = head.y
    let tx = tail.x
    let ty = tail.y

    match hx, hy, tx, ty with
    | hx, hy, tx, ty when hx > tx && hy = ty -> Right
    | hx, hy, tx, ty when hx < tx && hy = ty -> Left
    | hx, hy, tx, ty when hx = tx && hy = ty -> Overlapp
    | hx, hy, tx, ty when hx = tx && hy > ty -> Up
    | hx, hy, tx, ty when hx = tx && hy < ty -> Down
    | hx, hy, tx, ty when hx > tx && hy > ty -> UpRight
    | hx, hy, tx, ty when hx < tx && hy > ty -> UpLeft
    | hx, hy, tx, ty when hx > tx && hy < ty -> DownRight
    | hx, hy, tx, ty when hx < tx && hy < ty -> DownLeft

let parseInputLine (row: string) =
    let items = row.Split(" ")

    let direction =
        match items[0] with
        | "R" -> Right
        | "U" -> Up
        | "L" -> Left
        | "D" -> Down
        | _ -> raise (ArgumentException($"wrong input {items[0]}"))

    { direction = direction
      amount = (int items[1]) }

let move (inputRope: Rope) (direction: Direction) : Rope =
    let initialOrientation = orientation inputRope.head inputRope.tail
    let rope = { inputRope with head = translate inputRope.head direction }
    
    match initialOrientation, direction with
    | Up, Up -> { rope with tail = translate rope.head Down }
    | Down, Down -> { rope with tail = translate rope.head Up }
    | Left, Left -> { rope with tail = translate rope.head Right }
    | Right, Right -> { rope with tail = translate rope.head Left }
    | UpRight, Up -> { rope with tail = translate rope.head Down }
    | UpRight, Right -> { rope with tail = translate rope.head Left }
    | UpLeft, Up -> { rope with tail = translate rope.head Down }
    | UpLeft, Left -> { rope with tail = translate rope.head Right }
    | DownRight, Down -> { rope with tail = translate rope.head Up }
    | DownRight, Right -> { rope with tail = translate rope.head Left }
    | DownLeft, Down -> { rope with tail = translate rope.head Up }
    | DownLeft, Left -> { rope with tail = translate rope.head Right }
    | _, _ -> rope

let viz h w (r: Rope) =
    for y in [ 0..h ] do
        for x in [ 0..w ] do
            Console.SetCursorPosition(x, w - y)

            if r.head.x = x && r.head.y = y && r.tail.x = x && r.tail.y = y then
                Console.Write('X')
            else if r.head.x = x && r.head.y = y then
                Console.Write('H')
            else if r.tail.x = x && r.tail.y = y then
                Console.Write('T')
            else
                Console.Write('.')

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines($"{__SOURCE_DIRECTORY__}/input")

    let movements = input |> Array.map parseInputLine |> List.ofArray

    let r, _ =
        ({ head = { x = 0; y = 0 }
           tail = { x = 0; y = 0 } },
         movements)
        ||> List.mapFold (fun rope mov ->
            (rope, [ 1 .. mov.amount ])
            ||> List.mapFold (fun (r: Rope) _ ->
                let m = move r mov.direction
                m, m))

    let visited = r |> List.concat |> List.map (fun x -> x.tail) |> Set.ofList
    Console.WriteLine(visited.Count)
    0
