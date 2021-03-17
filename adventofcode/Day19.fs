module Day19

[<Literal>]
let InputFile = "Day19Input.txt"

type Replacement = { In: string; Out: string }

let parse (s: string) =
    s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    |> fun a -> { In = a.[0]; Out = a.[2] }

let getAllMolecules (s: string) (r: Replacement) =
    seq {
        for i in 0 .. s.Length - r.In.Length do
            if s.[i..].Substring(0, r.In.Length) = r.In then
                let s' =
                    s.[0..i - 1] + r.Out + s.[i + r.In.Length..]

                yield s'
    }

let day19 () =
    let lines = InputFile |> System.IO.File.ReadAllLines
    let ruleLines = lines.[0..lines.Length - 3]
    let molecule = Array.last lines
    let replacements = ruleLines |> Array.map parse

    replacements
    |> Array.map (getAllMolecules molecule)
    |> Seq.concat
    |> Seq.distinct
    |> Seq.length
