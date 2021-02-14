// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Day01

[<EntryPoint>]
let main argv =
    let r = Day01.day01()
    printfn "%A" r 
    0 // return an integer exit code