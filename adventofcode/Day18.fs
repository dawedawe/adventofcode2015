module Day18

[<Literal>]
let InputFile = "Day18Input.txt"

let getOnNeighbours xPos yPos (grid: char [] []) =
    let lowerXBound = max (xPos - 1) 0
    let lowerYBound = max (yPos - 1) 0
    let upperXBound = min (xPos + 1) (grid.[0].Length - 1)
    let upperYBound = min (yPos + 1) (grid.Length - 1)

    seq {
        for x in lowerXBound .. upperXBound do
            for y in lowerYBound .. upperYBound do
                if (x, y) <> (xPos, yPos) then
                    yield (x, y)
    }
    |> Seq.sumBy (fun (x, y) -> if grid.[y].[x] = '#' then 1 else 0)

let calcNextState x y (grid: char [] []) =
    let onNeighbours = getOnNeighbours x y grid

    match (grid.[y].[x], onNeighbours) with
    | '#', 2 -> '#'
    | '#', 3 -> '#'
    | '#', _ -> '.'
    | '.', 3 -> '#'
    | '.', _ -> '.'
    | _, _ -> failwith "bad state"

let animate n (input: string []) =
    let rec helper left grid =
        if left = 0 then
            grid
        else
            let left' = left - 1
            let grid' = Array.copy grid |> Array.map Array.copy

            for x in 0 .. grid.[0].Length - 1 do
                for y in 0 .. grid.Length - 1 do
                    grid'.[y].[x] <- calcNextState x y grid

            helper left' grid'

    helper n (input |> Array.map (fun s -> s.ToCharArray()))

let countOn (grid: char [] []) =
    grid
    |> Array.sumBy (Array.sumBy (fun c -> if c = '#' then 1 else 0))

let day18 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> animate 100
    |> countOn
