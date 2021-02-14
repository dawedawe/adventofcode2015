module Day02

[<Literal>]
let InputFile = "Day02Input.txt"

let parse (s: string) = s.Split('x') |> Array.map int

let calc (sizes: int []) =
    let l, w, h = sizes.[0], sizes.[1], sizes.[2]
    let sides = [| 2 * l * w; 2 * w * h; 2 * h * l |]
    let min = Array.min sides / 2
    Array.sum sides + min

let day02 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.sumBy (parse >> calc)

let calcPart2 (sizes: int []) =
    let l, w, h = sizes.[0], sizes.[1], sizes.[2]
    let volume = l * w * h

    let sideLength =
        sizes
        |> Array.sort
        |> Array.take 2
        |> Array.sumBy (fun x -> x * 2)

    sideLength + volume

let day02Part2 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.sumBy (parse >> calcPart2)
