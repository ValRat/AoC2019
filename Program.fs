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
let preProcess(tape: int[], noun, verb) = 
    Array.set tape 1 noun
    Array.set tape 2 verb
    tape

let preProcessPartOne (x: int[]) =
    preProcess(x, 12, 2)

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
        
let evaluateAndGetResult(tape, noun, verb) =
    let program = preProcess(tape, noun, verb)
    let result, _ = evaluate(Some program, 0)
    if result.IsNone
        then None
    else
        Some result.Value.[0]
    

let doSecondProblem = 
    let input = readInput(@"C:\Programming\adventofcode2019\AoC\inputs\2.txt") |> Seq.head 
    let tape = input.Split ',' |> Array.map int |> preProcessPartOne
    let result, _ = evaluate(Some tape, 0)
    if result.IsNone then
        printf "Failed to parse tape successfully"
    else
        printf "End Result:\n%A" result.Value


let doSecondProblemPartTwo = 
    let input = readInput(@"C:\Programming\adventofcode2019\AoC\inputs\2.txt") |> Seq.head 
    let tape = input.Split ',' |> Array.map int 

    let compare(tape, noun, verb) = 
        let value = evaluateAndGetResult(tape, noun, verb)
        if value.IsNone then
            false
        else
            value.Value = 19690720

    // Create generator of all possible input space
    let allNounVerbCombos = seq {
            for noun in 0..99 do
                for verb in 0..99 do
                    (noun, verb)
            }
        
    // Why do I have to make an array copy? I thought this is immutable? 
    // Ans: Lists are immutable, Arrays are not
    // Should prefer to use F#'s immutable list rather than arrays
    // TODO: Organize the code a bit better
    let desiredInputs = allNounVerbCombos |> Seq.tryFind (fun  (noun, verb) -> compare(Array.copy tape, noun, verb)) 
    if desiredInputs.IsNone then
        printf "No value found, an error probably occured"
    else
        printfn "(Noun: %d, Verb: %d)" <|| desiredInputs.Value

            

[<EntryPoint>]
let main argv =
    let output = doSecondProblemPartTwo
    0 // return an integer exit code
