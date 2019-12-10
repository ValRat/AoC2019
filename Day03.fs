module Day03

open System.IO
open System.Collections.Generic

let input = File.ReadAllLines(@"./inputs/3test.txt")

let sumTuple(a: int * int, b: int * int) =
    let a1, a2 = a
    let b1, b2 = b
    (a1+b1, a2+b2)

let Part1 = 
    let mutable grid = Map.empty

    let traceWire (segments: string[]) =
        let mutable currCoord = (0, 0)

        let doTraceSegment(dir: int * int, numSteps) =
            for x in 1..numSteps do
                currCoord <- sumTuple(currCoord, dir)
                if grid.ContainsKey(currCoord) then
                    grid <- grid.Add(currCoord, grid.TryFind(currCoord).Value + 1)
                else
                    grid <- grid.Add(currCoord, 1)
        ()

        let traceSegment(seg: string) = 
            match seg.[0] with
            | 'U' -> doTraceSegment((0, 1), seg.[1..] |> int)
            | 'D' -> doTraceSegment((0, -1), seg.[1..] |> int)
            | 'L' -> doTraceSegment((-1, 0), seg.[1..] |> int)
            | 'R' -> doTraceSegment((0, 1), seg.[1..] |> int)
            | _ -> ()

        Array.iter traceSegment segments
    
    input |> Array.map (fun s -> s.Split ',' |> traceWire) |> ignore

    let findShortestIntersection =

        let getDistance(kvp: KeyValuePair<int*int,_>) = 
            let a1, a2 = kvp.Key
            abs(a1) + abs(a2)

        let shortestIntersection = grid |> Map.filter (fun _ v -> v > 1) 
                                        |> Seq.minBy getDistance
                                        |> getDistance

        printfn "%d" shortestIntersection

    findShortestIntersection |> ignore


