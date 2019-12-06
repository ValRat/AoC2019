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

[<EntryPoint>]
let main argv =
    let output = doFirstProblem
    printf "%d" output
    0 // return an integer exit code
