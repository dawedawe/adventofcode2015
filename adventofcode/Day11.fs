module Day11

let increment (c: char) =
    let c' = ((int c % 97) + 1) % 26
    c' + 97 |> char

let incrementPw (pw: char []) =
    let wrapped = Array.create pw.Length false
    pw.[pw.Length - 1] <- increment pw.[pw.Length - 1]
    wrapped.[pw.Length - 1] <- pw.[pw.Length - 1] = 'a'

    for i in pw.Length - 2 .. -1 .. 0 do
        if wrapped.[i + 1] then
            pw.[i] <- increment pw.[i]
            wrapped.[i] <- pw.[i] = 'a'

    pw

let hasIncreasingStraight (pw: char []) =
    Array.windowed 3 pw
    |> Array.tryFind
        (fun a ->
            int a.[0] + 1 = int a.[1]
            && int a.[0] + 2 = int a.[2])
    |> Option.isSome

let hasNoInvalidChars (pw: char []) =
    (Array.contains 'i' pw
     || Array.contains 'o' pw
     || Array.contains 'l' pw)
    |> not

let hasPair (pw: char []) =
    seq {
        for i in 1 .. pw.Length - 1 do
            if pw.[i] = pw.[i - 1] then yield i
    }
    |> fun s -> not (Seq.isEmpty s) && Seq.max s - Seq.min s >= 2

let isValid (pw: char []) =
    hasIncreasingStraight pw
    && hasNoInvalidChars pw
    && hasPair pw

let rec findNextPw (pw: char []) =
    if isValid pw then
        pw
    else
        incrementPw pw |> findNextPw

let day11 () =
    "hxbxwxba".ToCharArray()
    |> findNextPw
    |> System.String
