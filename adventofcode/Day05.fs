module Day05

[<Literal>]
let InputFile = "Day05Input.txt"

let has3Vowels (s: string) =
    s.ToCharArray()
    |> Array.filter
        (fun c ->
            c = 'a'
            || c = 'e'
            || c = 'i'
            || c = 'o'
            || c = 'u')
    |> Array.length
    >= 3

let hasDoubleLetter (s: string) =
    let rec helper pos =
        if pos >= s.Length then false
        else if s.[pos - 1] = s.[pos] then true
        else helper (pos + 1)

    helper 1

let hasNoDisallowedSubstrings (s: string) =
    let subs = [ "ab"; "cd"; "pq"; "xy" ]

    seq {
        for sub in subs do
            if s.Contains(sub) then yield true
    }
    |> Seq.contains true
    |> not

let isNice (s: string) =
    has3Vowels s
    && hasDoubleLetter s
    && hasNoDisallowedSubstrings s

let day05 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.filter isNice
    |> Array.length
