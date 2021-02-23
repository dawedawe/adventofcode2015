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

let hasPair (s: string) =
    let rec helper idx0 idx1 =
        if idx1 >= s.Length - 2 then
            false
        else
            let pair = sprintf "%c%c" s.[idx0] s.[idx1]

            if s.Substring(idx1 + 1).Contains(pair) then
                true
            else
                helper (idx0 + 1) (idx1 + 1)

    helper 0 1

let hasRepeat (s: string) =
    let rec helper idx0 idx1 =
        if idx1 >= s.Length then false
        else if s.[idx0] = s.[idx1] then true
        else helper (idx0 + 1) (idx1 + 1)

    helper 0 2

let isNicePart2 (s: string) = hasPair s && hasRepeat s


let day05Part2 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.filter isNicePart2
    |> Array.length
