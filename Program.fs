open System
open System.IO

// Generic all problems
let readInput (i : string) = File.ReadAllLines(i) 

// First problem 
let calculateFuel (x: int) = (x / 3) - 2  
let calculateFuelCapped (x) = if calculateFuel x > 0 then calculateFuel x else 0

let rec recCalculateFuel (x) = 
    let sumFuel = x |> Array.sum
    if sumFuel = 0 then
        sumFuel
    else
        let currRequired = x |> Array.map calculateFuelCapped
        (currRequired |> Array.sum) + recCalculateFuel(currRequired)

let doFirstProblem = 
    let input = readInput(@"C:\Programming\adventofcode2019\AoC\inputs\1.txt")
    let result = input |> Array.map int |> recCalculateFuel
    result

// Second Problem
let preProcess (x: int[]) =
    Array.set x 1 12 
    Array.set x 2 2
    x

// Test-driving Options
// second part of tuple is wheter or not to continue processing
let evaluateSumOp(tape: int[], cmdPtr) = 
    try 
        Array.set tape tape.[cmdPtr+3] (tape.[tape.[cmdPtr+1]] + tape.[tape.[cmdPtr+2]]) 
        Some tape, true
    with _ -> None, false

let evaluateMultOp(tape: int[], cmdPtr) =
    try 
        Array.set tape tape.[cmdPtr+3] (tape.[tape.[cmdPtr+1]] * tape.[tape.[cmdPtr+2]])
        Some tape, true
    with _ -> None, false

let evaluateTerminate(tape: int[], cmdPtr) = 
    Some tape, false

let evaluateError(tape: int[], cmdPtr) = 
    None, false

let rec evaluate(tape: Option<int []>, cmdPtr) = 
    if tape.IsNone
        then None, false
    else
        match tape.Value.[cmdPtr] with
        | 1 -> 
            let newTape, cont = evaluateSumOp(tape.Value, cmdPtr)
            if cont 
                then evaluate(newTape, cmdPtr + 4)
            else
                newTape, false
        | 2 -> 
            let newTape, cont = evaluateMultOp(tape.Value, cmdPtr)
            if cont 
                then evaluate(newTape, cmdPtr + 4)
            else 
                newTape, false
        | 99 -> 
            evaluateTerminate(tape.Value, cmdPtr + 4)
        | _ ->
            evaluateError(tape.Value, cmdPtr + 4)
        


let doSecondProblem = 
    let input = readInput(@"C:\Programming\adventofcode2019\AoC\inputs\2.txt") |> Seq.head 
    let tape = input.Split ',' |> Array.map int |> preProcess
    let result, _ = evaluate(Some tape, 0)
    if result.IsNone
        then printf "Failed to parse tape successfully"
    else
        printf "End Result:\n%A" result.Value


[<EntryPoint>]
let main argv =
    let output = doSecondProblem 
    0 // return an integer exit code
