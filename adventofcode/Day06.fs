module Day06

open System.Text.RegularExpressions

[<Literal>]
let InputFile = "Day06Input.txt"

type CoordPair = (int * int)

type Instruction =
    | TurnOn of corner1: CoordPair * corner2: CoordPair
    | TurnOff of corner1: CoordPair * corner2: CoordPair
    | Toggle of corner1: CoordPair * corner2: CoordPair

let parse (s: string) =
    let regex =
        Regex(@"\w* (\d*),(\d*) through (\d*),(\d*)")

    let matches = regex.Matches(s)

    let corner1 =
        (int matches.[0].Groups.[1].Value, int matches.[0].Groups.[2].Value)

    let corner2 =
        (int matches.[0].Groups.[3].Value, int matches.[0].Groups.[4].Value)

    if s.StartsWith("turn on") then
        TurnOn(corner1, corner2)
    else if s.StartsWith("turn off") then
        TurnOff(corner1, corner2)
    else if s.StartsWith("toggle") then
        Toggle(corner1, corner2)
    else
        failwith "bad input"

let gridTransform (grid: bool [,]) ((c1x, c1y), (c2x, c2y)) f =
    for x = c1x to c2x do
        for y = c1y to c2y do
            grid.[x, y] <- f grid.[x, y]

    grid

let doInstruction (grid: bool [,]) instr =
    match instr with
    | TurnOn (c1, c2) -> gridTransform grid (c1, c2) (fun _ -> true)
    | TurnOff (c1, c2) -> gridTransform grid (c1, c2) (fun _ -> false)
    | Toggle (c1, c2) -> gridTransform grid (c1, c2) not

let day06 () =
    let grid =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse
        |> Array.fold doInstruction (Array2D.create 1000 1000 false)

    seq {
        for x = 0 to 999 do
            for y = 0 to 999 do
                if grid.[x, y] then yield 1
    }
    |> Seq.length

let gridTransformPart2 (grid: int [,]) ((c1x, c1y), (c2x, c2y)) f =
    for x = c1x to c2x do
        for y = c1y to c2y do
            grid.[x, y] <- f grid.[x, y]

    grid

let doInstructionPart2 (grid: int [,]) instr =
    match instr with
    | TurnOn (c1, c2) -> gridTransformPart2 grid (c1, c2) (fun x -> x + 1)
    | TurnOff (c1, c2) -> gridTransformPart2 grid (c1, c2) (fun x -> max (x - 1) 0)
    | Toggle (c1, c2) -> gridTransformPart2 grid (c1, c2) (fun x -> x + 2)

let day06Part2 () =
    let grid =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse
        |> Array.fold doInstructionPart2 (Array2D.create 1000 1000 0)

    seq {
        for x = 0 to Array2D.length1 grid - 1 do
            yield Array.sum grid.[x, *]
    }
    |> Seq.sum
