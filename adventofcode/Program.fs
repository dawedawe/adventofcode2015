// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

[<EntryPoint>]
let main argv =
    let r = Day13.day13()
    printfn "%A" r 
    0 // return an integer exit code