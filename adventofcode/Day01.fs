module Day01

[<Literal>]
let InputFile = "Day01Input.txt"

let day01 () =
    InputFile
    |> System.IO.File.ReadAllText
    |> fun s -> s.ToCharArray()
    |> Array.countBy (fun c -> if c = '(' then 1 else (-1))
    |> Array.sumBy (fun x -> fst x * snd x)
