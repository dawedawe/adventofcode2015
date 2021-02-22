module Day04

[<Literal>]
let Input = "ckczppom"

let hash (s: string) =
    let h =
        s.ToCharArray()
        |> Array.map byte
        |> System.Security.Cryptography.MD5.HashData

    (h.[0], h.[1], h.[2] >>> 4)

let find hashF (s: string) =
    let rec helper i =
        let s' = s + string i
        let h = hashF s'

        match h with
        | (0uy, 0uy, 0uy) -> i
        | _ -> helper (i + 1)

    helper 1

let day04 () = find hash Input

let hashPart2 (s: string) =
    let h =
        s.ToCharArray()
        |> Array.map byte
        |> System.Security.Cryptography.MD5.HashData

    (h.[0], h.[1], h.[2])

let day04Part2 () = find hashPart2 Input
