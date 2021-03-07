module Day14

[<Literal>]
let InputFile = "Day14Input.txt"

type Reindeer =
    { Name: string
      Speed: int
      FlyTime: int
      FlyTimeLeft: int
      SleepTime: int
      SleepTimeLeft: int
      Distance: int
      Points: int }

let parse (s: string) =
    let words = s.Split(' ')

    { Name = words.[0]
      Speed = int words.[3]
      FlyTime = int words.[6]
      FlyTimeLeft = int words.[6]
      SleepTime = int words.[13]
      SleepTimeLeft = int words.[13]
      Distance = 0
      Points = 0 }

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

let rec calcSecond (r: Reindeer) =
    if r.FlyTimeLeft > 0 then
        { r with
              Distance = r.Distance + r.Speed
              FlyTimeLeft = r.FlyTimeLeft - 1 }
    else if r.SleepTimeLeft > 0 then
        { r with
              SleepTimeLeft = r.SleepTimeLeft - 1 }
    else if r.FlyTimeLeft = 0 && r.SleepTimeLeft = 0 then
        { r with
              FlyTimeLeft = r.FlyTime
              SleepTimeLeft = r.SleepTime }
        |> calcSecond
    else
        failwith "bad state"

let rec run (secs: int) (reindeers: Reindeer []) =
    if secs = 0 then
        reindeers
    else
        let reindeers' = reindeers |> Array.map calcSecond

        let maxDist =
            reindeers'
            |> Array.sortByDescending (fun r -> r.Distance)
            |> Array.head
            |> fun r -> r.Distance

        let reindeers'' =
            reindeers'
            |> Array.map
                (fun r ->
                    if r.Distance = maxDist then
                        { r with Points = r.Points + 1 }
                    else
                        r)

        run (secs - 1) reindeers''

let day14Part2 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map parse
    |> run 2503
    |> Array.maxBy (fun r -> r.Points)
    |> fun r -> r.Points
