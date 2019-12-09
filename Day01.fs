module Day01

open System.IO

// First problem 
let input = File.ReadAllLines(@".\inputs\1.txt")
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
    let result = input |> Array.map int |> recCalculateFuel
    result

// Compartmentalize
let Part1 = 
    let result = input |> Array.map int |> Array.sumBy calculateFuel
    printfn "Day1 Part1: %d\n" result

let Part2 =
    let result = input |> Array.map int |> recCalculateFuel
    printfn "Day1 Part2: %d\n" result