// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

[<EntryPoint>]
let main argv =
    let r = Day04.day04Part2()
    printfn "%A" r 
    0 // return an integer exit code