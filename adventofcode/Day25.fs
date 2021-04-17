module Day25

[<Literal>]
let InputFile = "Day25Input.txt"

let parse (s: string) =
    let words = s.Split(' ')
    words
    |> Array.map (fun w -> w.Trim(',').Trim('.'))
    |> Array.filter (fun w -> System.Int32.TryParse w |> fun (r, _) -> r)
    |> fun a -> (int a.[0], int a.[1])

let calc (prev: int64) =
    (prev * 252533L) % 33554393L

let calcIndex idx =
    let mutable prev = 20151125L
    if idx = 1
    then prev
    else
        for _ in 2 .. idx do
             prev <- calc prev
        prev

let indexAtRowCol (row, col) =
    let mutable prevRow = 1
    let mutable prevVal = 1
    if row > 1 then
        for r in 2 .. row do
            prevVal <- prevRow + prevVal
            prevRow <- r
    if col > 1 then
        for c in 2 .. col do
            prevVal <- prevVal + row + c - 1
    prevVal

let day25 () =
    InputFile
    |> System.IO.File.ReadAllText
    |> parse
    |> indexAtRowCol
    |> calcIndex