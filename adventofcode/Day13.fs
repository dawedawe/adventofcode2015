module Day13

[<Literal>]
let InputFile = "Day13Input.txt"

type Rule =
    { Subject: string
      NextTo: string
      Change: int }

let parse (s: string) =
    let a = s.Trim('.').Split(' ')
    let f = if a.[2] = "gain" then (+) else (-)

    { Subject = a.[0]
      NextTo = a.[10]
      Change = (f) 0 (int a.[3]) }

let rec getPermutations (a: string []) =
    match a with
    | [||] -> [| a |]
    | [| _ |] -> [| a |]
    | _ ->
        seq {
            for x in a do
                let a' = Array.filter ((<>) x) a

                for a'' in getPermutations a' do
                    yield (Array.append [| x |] a'')
        }
        |> Seq.toArray

let calcHappyness (rules: Rule []) (seating: string []) =
    let len = seating.Length

    seq {
        for i in 0 .. len - 1 do
            let left = seating.[(i - 1 + len) % len]
            let right = seating.[(i + 1 + len) % len]

            let leftRule =
                rules
                |> Array.find (fun r -> r.Subject = seating.[i] && r.NextTo = left)

            let rightRule =
                rules
                |> Array.find (fun r -> r.Subject = seating.[i] && r.NextTo = right)

            yield leftRule.Change
            yield rightRule.Change
    }
    |> Seq.sum

let day13 () =
    let rules =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse

    let subs =
        rules
        |> Array.map (fun r -> r.Subject)
        |> Array.distinct

    getPermutations subs
    |> Array.map (calcHappyness rules)
    |> Array.max
