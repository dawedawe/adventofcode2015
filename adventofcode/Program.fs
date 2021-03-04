// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

[<EntryPoint>]
let main argv =
    let r = Day12.day12()
    printfn "%A" r 
    0 // return an integer exit code