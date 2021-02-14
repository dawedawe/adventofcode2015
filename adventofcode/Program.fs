// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Day02

[<EntryPoint>]
let main argv =
    let r = Day02.day02()
    printfn "%A" r 
    0 // return an integer exit code