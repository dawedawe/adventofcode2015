module Day14

[<Literal>]
let InputFile = "Day14Input.txt"

type Reindeer =
    { Name: string
      Speed: int
      FlyTime: int
      SleepTime: int }

let parse (s: string) =
    let words = s.Split(' ')

    { Name = words.[0]
      Speed = int words.[3]
      FlyTime = int words.[6]
      SleepTime = int words.[13] }

let calcDistance (s: int) (reindeer: Reindeer) =
    let rec helper sofar secs =
        if secs = 0 then
            sofar
        else
            let timeInFlight = min secs reindeer.FlyTime
            let distance = sofar + timeInFlight * reindeer.Speed
            let secs' = secs - timeInFlight
            let timeInRest = min secs' reindeer.SleepTime
            let secs'' = secs' - timeInRest
            helper distance secs''

    helper 0 s

let day14 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map parse
    |> Array.map (calcDistance 2503)
    |> Array.max
