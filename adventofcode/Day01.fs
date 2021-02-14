module Day01

[<Literal>]
let InputFile = "Day01Input.txt"

let day01 () =
    InputFile
    |> System.IO.File.ReadAllText
    |> fun s -> s.ToCharArray()
    |> Array.countBy (fun c -> if c = '(' then 1 else (-1))
    |> Array.sumBy (fun x -> fst x * snd x)

let rec move currentFloor c position =
    if (Array.isEmpty c) then
        failwith "empty input"

    let currentFloor' =
        if c.[0] = '(' then
            currentFloor + 1
        else
            currentFloor - 1

    let position' = position + 1

    if currentFloor' = -1 then
        position'
    else
        move currentFloor' c.[1..] position'

let day01Part2 () =
    let input =
        InputFile
        |> System.IO.File.ReadAllText
        |> fun s -> s.ToCharArray()

    move 0 input 0
