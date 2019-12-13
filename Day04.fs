module Day04

open System

type MinMax = {min: int; max: int}
let input = {min = 356261; max = 846303}

let adjacencyCheck(pass: int) =
    pass.ToString() |> Seq.pairwise |> Seq.exists (fun e -> fst(e) = snd(e))

let neverDecreaseCheck(pass: int) = 
    pass.ToString() |> Seq.pairwise |> Seq.forall (fun e -> fst(e) <= snd(e))

let adjacencyGroupingCheck(pass: int) =
    pass.ToString() |> Seq.pairwise 
                    |> Seq.where (fun e -> fst(e) = snd(e))
                    |> Seq.groupBy id
                    |> Seq.exists (fun grp -> snd(grp) 
                                                |> Seq.length 
                                                |> (fun len -> len = 1))

let evaluatePassword(checks) =
    let allNumbersInRange = seq {
        for pass in input.min..input.max do
            if (checks |> List.map (fun check -> check pass) |> Seq.forall id) then
                yield pass
    }

    let potentialPasswords = allNumbersInRange |> Seq.length
    printfn "Num of potential passwords: %d" potentialPasswords

let Part1 = 
    evaluatePassword([adjacencyCheck; neverDecreaseCheck])

let Part2 = 
    evaluatePassword([adjacencyGroupingCheck; neverDecreaseCheck])