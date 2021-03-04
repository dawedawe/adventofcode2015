module Day12

open System.Text.Json

[<Literal>]
let InputFile = "Day12Input.txt"

let sum (s: string) =
    let symbols = s.ToCharArray()

    seq {
        for i in 0 .. symbols.Length - 2 do

            if System.Char.IsDigit(symbols.[i])
               || symbols.[i] = '-' then
                yield symbols.[i]

                if not (System.Char.IsDigit(symbols.[i + 1])) then
                    yield ' '
    }
    |> Seq.toArray
    |> System.String
    |> fun x -> x.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.sumBy int

let day12 () =
    InputFile |> System.IO.File.ReadAllText |> sum

let hasRedProperty (jo: JsonElement) =
    jo.EnumerateObject()
    |> Seq.tryFind
        (fun p ->
            p.Value.ValueKind = JsonValueKind.String
            && p.Value.GetString() = "red")
    |> Option.isSome

let rec sumObject (jo: JsonElement) =
    if hasRedProperty jo then
        0
    else
        seq {
            for p in jo.EnumerateObject() do
                match p.Value.ValueKind with
                | JsonValueKind.Array -> yield sumArray p.Value
                | JsonValueKind.Object -> yield sumObject p.Value
                | JsonValueKind.String -> yield 0
                | JsonValueKind.Number -> yield p.Value.GetInt32()
                | _ -> failwith "unsupported ValueKind"
        }
        |> Seq.sum

and sumArray (ja: JsonElement) =
    seq {
        for je in ja.EnumerateArray() do
            match je.ValueKind with
            | JsonValueKind.Array -> yield sumArray je
            | JsonValueKind.Object -> yield sumObject je
            | JsonValueKind.String -> yield 0
            | JsonValueKind.Number -> yield je.GetInt32()
            | _ -> failwith "unsupported ValueKind"
    }
    |> Seq.sum

let sum2 (je: JsonElement) =
    match je.ValueKind with
    | JsonValueKind.Array -> sumArray je
    | JsonValueKind.Object -> sumObject je
    | JsonValueKind.String -> 0
    | JsonValueKind.Number -> je.GetInt32()
    | _ -> failwith "unsupported ValueKind"

let day12Part2 () =
    InputFile
    |> System.IO.File.ReadAllText
    |> System.Text.Json.JsonDocument.Parse
    |> fun d -> sum2 d.RootElement
