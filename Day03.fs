module Day03

open System.IO
open System.Collections.Generic

let input = File.ReadAllLines(@"./inputs/3.txt")

let sumTuple(a: int * int, b: int * int) =
    let a1, a2 = a
    let b1, b2 = b
    (a1+b1, a2+b2)

let Part1 = 
    let mutable grid: Map<int*int, Set<int>> = Map.empty

    let traceWire (segments) =
        let (segs: string[], id: int) = segments

        let mutable currCoord = (0, 0)

        let doTraceSegment(dir: int * int, numSteps) =
            for _ in 1..numSteps do
                currCoord <- sumTuple(currCoord, dir)
                if grid.ContainsKey(currCoord) then
                    grid <- grid.Add(currCoord, grid.TryFind(currCoord).Value.Add(id))
                else
                    grid <- grid.Add(currCoord, Set.empty.Add(id))

        let traceSegment(seg: string) = 
            match seg.[0] with
            | 'U' -> doTraceSegment(( 0,  1), seg.[1..] |> int)
            | 'D' -> doTraceSegment(( 0, -1), seg.[1..] |> int)
            | 'L' -> doTraceSegment((-1,  0), seg.[1..] |> int)
            | 'R' -> doTraceSegment(( 1,  0), seg.[1..] |> int)
            | _ -> failwith "UNRECOGNIZED VALUE"

        Array.iter traceSegment segs

    input |> Array.map (fun s -> s.Split ',') |> Array.mapi (fun id s -> (s, id)) |> Array.iter traceWire |> ignore


    // Having to define the types for these means I'm probably doing something wrong...
    let findShortestIntersection(inputGrid: Map<int*int, Set<int>>) =
        let absSumTuple(tup) = 
            let (t1, t2) = tup
            abs(t1) + abs(t2)

        let getDistance(kvp: KeyValuePair<int*int,_>) = 
            absSumTuple(kvp.Key)

        let shortestIntersection = inputGrid |> Map.filter (fun _ v -> v.Count > 1)  // Find intersections
                                        // |> Map.filter (fun k _-> (k |> absSumTuple) = 0)  // Filter out origin if needed
                                        |> Seq.minBy getDistance
                                        |> getDistance
        shortestIntersection

    let closestIntersection = findShortestIntersection(grid) 
    printfn "%d" closestIntersection