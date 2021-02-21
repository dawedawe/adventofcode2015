module Day03

[<Literal>]
let InputFile = "Day03Input.txt"

let move (line: string) =
    let rec helper positions (x, y) moves =
        if Array.isEmpty moves then
            positions
        else
            let newPos =
                match moves.[0] with
                | '<' -> (x - 1, y)
                | '^' -> (x, y + 1)
                | 'v' -> (x, y - 1)
                | '>' -> (x + 1, y)
                | _ -> failwith "bad input"

            let positions' = Set.add newPos positions
            helper positions' newPos moves.[1..]

    let startingPos = (0, 0)
    let positions = Set.singleton startingPos
    helper positions startingPos (line.ToCharArray())

let day03 () =
    InputFile
    |> System.IO.File.ReadAllText
    |> move
    |> Set.count

let day03Part2 () =
    let (part0, part1) =
        InputFile
        |> System.IO.File.ReadAllText
        |> fun s -> s.ToCharArray()
        |> Array.indexed
        |> Array.partition (fun (i, _) -> i % 2 = 0)

    let input0 = part0 |> Array.map snd |> System.String
    let input1 = part1 |> Array.map snd |> System.String
    let pos0 = move input0
    let pos1 = move input1
    Set.union pos0 pos1 |> Set.count
