module Day17

[<Literal>]
let InputFile = "Day17Input.txt"

let getSubset (containers: (int * int) []) i =
    seq {
        for (idx, c) in containers do
            let take = i >>> idx &&& 0x01
            if (take = 1) then yield c
    }

let findCombinations (containers: int []) =
    let upperBound =
        int (2. ** (float (Array.length containers))) - 1

    let containers' = Array.indexed containers

    seq {
        for i in 0 .. upperBound do
            let subSet = getSubset containers' i
            if Seq.sum subSet = 150 then yield 1
    }
    |> Seq.sum

let day17 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map int
    |> findCombinations
