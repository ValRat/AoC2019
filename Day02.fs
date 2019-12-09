module Day02

open System.IO

// Second Problem
let input = File.ReadAllLines(@".\inputs\2.txt") |> Seq.head 

let preProcess(tape: int[], noun, verb) = 
    Array.set tape 1 noun
    Array.set tape 2 verb
    tape

let preProcessPartOne (x: int[]) =
    preProcess(x, 12, 2)

// **** VM CODE *****
// Trying out Options
// Second part of tuple is wheter or not to continue processing
// Tape inspired by Turing Machine
// TODO: Evaluate whether this will be better with returning immutable lists
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

// ****** END OF VM CODE ******
        
let evaluateAndGetResult(tape, noun, verb) =
    let program = preProcess(tape, noun, verb)
    let result, _ = evaluate(Some program, 0)
    if result.IsNone
        then None
    else
        Some result.Value.[0]
    

let Part1 = 
    let tape = input.Split ',' |> Array.map int |> preProcessPartOne
    let result, _ = evaluate(Some tape, 0)
    if result.IsNone then
        printfn "Failed to parse tape successfully"
    else
        printfn "Day2 Part1:\nEnd Result:\n%A" result.Value


let Part2 = 
    let tape = input.Split ',' |> Array.map int 

    let isCorrect(tape, noun, verb) = 
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
    let desiredInputs = allNounVerbCombos |> Seq.tryFind (fun  (noun, verb) -> isCorrect(Array.copy tape, noun, verb)) 
    printfn "Day 2 Part 2:"
    if desiredInputs.IsNone then
        printfn "No value found, an error probably occured\n"
    else
        printfn "(Noun: %d, Verb: %d)" <|| desiredInputs.Value

            