open System
open System.IO
open System.Text



type FileSystemItem =
    | FileItem of FileItem
    | DirectoryItem of DirectoryItem

and FileItem = { name: string; size: int }

and DirectoryItem =
    { name: string
      size: int
      items: FileSystemItem list }


type CD =
    | GotoRoot
    | GotoParent
    | GotoChild of string

type Command =
    | CD of CD
    | LS

type LsOutput =
    | File of int * string
    | Directory of string

type ConsoleOutput =
    | Command of Command
    | Output of LsOutput

let parseCd (cdarg: string) =
    match cdarg with
    | "/" -> GotoRoot
    | ".." -> GotoParent
    | _ -> GotoChild cdarg

let parseCommand (row: string) =
    let items = row.Split(" ")

    match items[1] with
    | "ls" -> LS
    | "cd" -> CD(parseCd (items[2]))
    | _ -> raise (InvalidProgramException($"yo, somethings wrong {items[1]} is not a command"))

let parseOutput (row: string) =
    let items = row.Split(" ")

    match items[0] with
    | "dir" -> Directory items[1]
    | num when num |> Seq.forall Char.IsDigit -> File(int items[0], items[1])
    | _ -> raise (InvalidProgramException($"wrong output input {items[0]}"))

let parseRow (row: string) =
    if not (row[0] = '$') then
        Output(parseOutput (row))
    else
        Command(parseCommand row)


let rec buildFileSystem (parentDirectory: DirectoryItem) (inputs: list<string>) =
    match inputs with
        | [] -> parentDirectory
        | _ ->
            match parseRow inputs.Head with
                | Command cmd ->
                    match cmd with
                        | LS -> parentDirectory
            
[<EntryPoint>]
let main arv =
    let input = File.ReadAllLines("/home/emile/programming/aoc22/src/aoc7/test_input")
    input |> Array.map (fun r -> Console.WriteLine(parseRow r)) |> ignore


    let res =
        ({ name = "root/"; size = 0; items = [] }, input)
        ||> Array.fold (fun acc line ->
            match (parseRow line) with
            | Command cmd ->
                        match cmd with
                        | CD c -> match c with
                            | GotoChild name -> List.find (fun c -> c.name = name) acc.items
            | Output o ->
                        match o with
                        | Directory d -> { acc with items = DirectoryItem { name = d; size = 0; items = [] } :: acc.items }
                        | File(s, n) -> { acc with items = FileItem { name = n; size = s } :: acc.items })

    Console.WriteLine(res)
    0
