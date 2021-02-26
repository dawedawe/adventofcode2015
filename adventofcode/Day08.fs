module Day08

[<Literal>]
let InputFile = "Day08Input.txt"

let inMemoryLength (line: string) =
    let rec helper (n: int) (s: string) =
        if s.Length <= 1 then
            n + s.Length
        else
            match (s.[0], s.[1]) with
            | ('\\', '\\') -> helper (n + 1) s.[2..]
            | ('\\', '"') -> helper (n + 1) s.[2..]
            | ('\\', 'x') -> helper (n + 1) s.[4..]
            | _ -> helper (n + 1) s.[1..]

    helper 0 line.[1..line.Length - 2]

let day08 () =
    let lines = InputFile |> System.IO.File.ReadAllLines
    let codeLength = lines |> Array.sumBy String.length
    let inMemLength = lines |> Array.sumBy inMemoryLength
    codeLength - inMemLength

let inCodeLength (line: string) =
    let rec helper n (s: string) =
        if s = "" then
            n + 2
        else if s.Length >= 1 && s.[0] = '"' then
            helper (n + 2) s.[1..]
        else if s.Length >= 2 && s.[0] = '\\' && s.[1] = '\\' then
            helper (n + 4) s.[2..]
        else if s.Length >= 2 && s.[0] = '\\' && s.[1] = '"' then
            helper (n + 4) s.[2..]
        else if s.Length >= 2 && s.[0] = '\\' && s.[1] = 'x' then
            helper (n + 3) s.[2..]
        else
            helper (n + 1) s.[1..]

    helper 0 line

let day08Part2 () =
    let lines = InputFile |> System.IO.File.ReadAllLines
    let inMemLength = lines |> Array.sumBy String.length
    let inCodeLength = lines |> Array.sumBy inCodeLength
    inCodeLength - inMemLength
