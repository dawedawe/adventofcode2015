module Day07

[<Literal>]
let InputFile = "Day07Input.txt"

type Input =
    | Wire of string
    | Value of uint16

let getWires inputs =
    seq {
        for i in inputs do
            match i with
            | Wire w -> yield w
            | _ -> ()
    }
    |> Seq.toList

let getInputValue (wireSignals: Map<string, uint16>) input =
    match input with
    | Wire w -> wireSignals.[w]
    | Value v -> v

type Gate =
    | And of in1: Input * in2: Input * outWire: string
    | Or of in1: Input * in2: Input * outWire: string
    | Lshift of inWire: string * value: int * outWire: string
    | Rshift of inWire: string * value: int * outWire: string
    | Not of inWire: string * out: string
    | Direct of Input * out: string

let getNeededInputWires gate =
    match gate with
    | And (in1, in2, _) -> getWires [ in1; in2 ]
    | Or (in1, in2, _) -> getWires [ in1; in2 ]
    | Lshift (inWire, _, _) -> [ inWire ]
    | Rshift (inWire, _, _) -> [ inWire ]
    | Not (inWire, _) -> [ inWire ]
    | Direct (in1, _) -> getWires [ in1 ]

let runGate (wireSignals: Map<string, uint16>) gate =
    match gate with
    | Rshift (inWire, value, outWire) -> (outWire, wireSignals.[inWire] >>> value)
    | Lshift (inWire, value, outWire) -> (outWire, wireSignals.[inWire] <<< value)
    | And (in1, in2, outWire) ->
        let val1 = getInputValue wireSignals in1
        let val2 = getInputValue wireSignals in2
        (outWire, val1 &&& val2)
    | Or (in1, in2, outWire) ->
        let val1 = getInputValue wireSignals in1
        let val2 = getInputValue wireSignals in2
        (outWire, val1 ||| val2)
    | Not (inWire, outWire) -> (outWire, ~~~wireSignals.[inWire])
    | Direct (input, outWire) -> (outWire, getInputValue wireSignals input)

let parseInput (s: string) =
    match System.UInt16.TryParse s with
    | true, i -> Value i
    | false, _ -> Wire s

let parse (s: string) =
    let strings = s.Split(' ')

    if s.Contains("RSHIFT") then
        Rshift(strings.[0], (int strings.[2]), strings.[4])
    else if s.Contains("LSHIFT") then
        Lshift(strings.[0], (int strings.[2]), strings.[4])
    else if s.Contains("AND") then
        And((parseInput strings.[0]), (parseInput strings.[2]), strings.[4])
    else if s.Contains("OR") then
        Or((parseInput strings.[0]), (parseInput strings.[2]), strings.[4])
    else if s.Contains("NOT") then
        Not(strings.[1], strings.[3])
    else
        Direct((parseInput strings.[0]), strings.[2])

let hasInputsReady wireSignals gate =
    let neededInputWires = getNeededInputWires gate

    neededInputWires
    |> List.forall (fun w -> Map.containsKey w wireSignals)

let updateGrid wireSignals gates gateToRun =
    let (outWire, outSignal) = runGate wireSignals gateToRun
    let wireSignals' = Map.add outWire outSignal wireSignals
    let gates' = Array.filter (fun g -> g <> gateToRun) gates
    (gates', wireSignals')

let runGrid =
    let rec helper wireSignals gatesToRun =
        if Array.isEmpty gatesToRun then
            wireSignals
        else
            let readyGates =
                gatesToRun
                |> Array.filter (hasInputsReady wireSignals)
            let (gates', wireSignals') =
                updateGrid wireSignals gatesToRun readyGates.[0]
            helper wireSignals' gates'

    helper Map.empty

let day07 () =
    let wireSignals =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse
        |> runGrid
    wireSignals.["a"]
