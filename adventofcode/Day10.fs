module Day10

let getRun (s: string) =
    s.ToCharArray()
    |> Array.takeWhile (fun c -> c = s.[0])
    |> fun a -> System.String.Join(null, a)

let transform (s: string) =
    if (s.Length > 9) then
        failwith "unsupported length"

    let count = s.Length.ToString()
    let digit = string s.[0]
    count + digit

let expand (s: string) =
    let rec helper (sofar: string) index =
        if index = s.Length then
            sofar
        else
            let toExpand = getRun s.[index..]
            let expanded = transform toExpand
            let sofar' = sofar + expanded
            let index' = (index + toExpand.Length)
            helper sofar' index'

    helper "" 0

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
