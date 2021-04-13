module Day23

[<Literal>]
let InputFile = "Day23Input.txt"

type Instruction =
    | Hlf of char
    | Tpl of char
    | Inc of char
    | Jmp of int
    | Jie of reg:char * off:int
    | Jio of reg:char * off:int

let parse (s: string) =
    let instruction = s.Substring(0, 3)
    match instruction with
    | "hlf" -> Hlf s.[4]
    | "tpl" -> Tpl s.[4]
    | "inc" -> Inc s.[4]
    | "jmp" -> Jmp (int s.[4..])
    | "jie" -> Jie (s.[4], (int s.[7..]))
    | "jio" -> Jio (s.[4], (int s.[7..]))
    | _ -> failwith "can't parse"

let exec instr ptr a b =
    match instr with
    | Hlf 'a' -> a / 2, b, ptr + 1
    | Hlf 'b' -> a, b / 2, ptr + 1
    | Tpl 'a' -> a * 3, b, ptr + 1
    | Tpl 'b' -> a, b * 3, ptr + 1
    | Inc 'a' -> a + 1, b, ptr + 1
    | Inc 'b' -> a, b + 1, ptr + 1
    | Jmp off -> a, b, ptr + off
    | Jie ('a', off) -> if (a % 2) = 0 then a, b, ptr + off else a, b, ptr + 1
    | Jie ('b', off) -> if (b % 2) = 0 then a, b, ptr + off else a, b, ptr + 1
    | Jio ('a', off) -> if a = 1 then a, b, ptr + off else a, b, ptr + 1
    | Jio ('b', off) -> if b = 1 then a, b, ptr + off else a, b, ptr + 1
    | _ -> failwith "can't exec"

let run (program: Instruction []) =
    let rec helper a b ptr =
        let a', b', ptr' = exec program.[ptr] ptr a b
        if ptr' < Array.length program
        then helper a' b' ptr'
        else b'
    helper 0 0 0

let day23 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map parse
    |> run
