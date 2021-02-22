module Day04

[<Literal>]
let Input = "ckczppom"

let hash (s: string) =
    let h =
        s.ToCharArray()
        |> Array.map byte
        |> System.Security.Cryptography.MD5.HashData

    (h.[0], h.[1], h.[2] >>> 4)

let find (s: string) =
    let rec helper i =
        let s' = s + string i
        let h = hash s'

        match h with
        | (0uy, 0uy, 0uy) -> i
        | _ -> helper (i + 1)

    helper 1

let day04 () = find Input
