module Day12

[<Literal>]
let InputFile = "Day12Input.txt"

let sum (s: string) =
    let symbols = s.ToCharArray()

    seq {
        for i in 0 .. symbols.Length - 2 do

            if System.Char.IsDigit(symbols.[i])
               || symbols.[i] = '-' then
                yield symbols.[i]

                if not (System.Char.IsDigit(symbols.[i + 1])) then
                    yield ' '
    }
    |> Seq.toArray
    |> System.String
    |> fun x -> x.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.sumBy int

let day12 () =
    InputFile |> System.IO.File.ReadAllText |> sum
