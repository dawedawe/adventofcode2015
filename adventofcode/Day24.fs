module Day24

[<Literal>]
let InputFile = "Day24Input.txt"

let getSubSetsOfSizeAndWeight size targetWeight (weights: int []) =
    let m = 2. ** float weights.Length |> int
    seq {
        for bitmask in 0 .. m - 1 do
            let indexesToTake = seq {
                for idx in 0 .. weights.Length - 1 do
                    if ((bitmask >>> idx) &&& 0x01) = 1
                    then yield idx
                }
            if Seq.length indexesToTake = size
            then
                let subset =
                    seq {
                        for idx in indexesToTake do
                            yield weights.[idx]
                    } |> Set.ofSeq
                if (subset |> Set.fold (+) 0) = targetWeight
                then yield subset
    } |> Set.ofSeq

let f groups (weights: int []) =
    let targetWeight = (weights |> Array.sum) / groups
    seq {
        for group0Count in 3 .. weights.Length - 2 do
            let group0Candidates = weights |> getSubSetsOfSizeAndWeight group0Count targetWeight
            for group0 in group0Candidates do
                yield group0
    }

let day24 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map int
    |> f 3
    |> Seq.head
    |> Seq.map int64
    |> Seq.reduce (*)

let day24Part2 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map int
    |> f 4
    |> Seq.head
    |> Seq.map int64
    |> Seq.reduce (*)
