module Day10

let getRun (s: string) =
    if s.Length >= 3 && s.[0] = s.[1] && s.[1] = s.[2]
    then s.Substring(0, 3)
    else if s.Length >= 2 && s.[0] = s.[1]
    then s.Substring(0, 2)
    else string s.[0]

let expand (s: string) =
    let b = System.Text.StringBuilder()
    let rec helper index =
        if index = s.Length then
            b.ToString()
        else
            let toExpand = getRun s.[index..]
            b.Append(toExpand.Length.ToString()) |> ignore
            b.Append(string toExpand.[0]) |> ignore
            helper (index + toExpand.Length)

    helper 0

let rec lookAndSay n input =
    if n = 0 then
        input
    else
        let input' = expand input
        lookAndSay (n - 1) input'

let day10 () =
    let input = "1321131112"
    let output = lookAndSay 40 input
    output.Length

let day10Part2 () =
    let input = "1321131112"
    let output = lookAndSay 50 input
    output.Length
