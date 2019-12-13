module Day06

open System.IO


// TODO: Make a graph?
// dynamic programming?, no data structure?

let input = File.ReadAllLines(@".\inputs\6.txt")


// In case I need this...
let initChildParentMap: Map<string, string> =
    let mutable tmpMap = Map.empty
    
    let getParentChild (x:string) = 
        let elts = x.Split(')') |> Seq.take 2 |> Seq.toList
        (elts.[1], elts.[0])

    input |> Seq.iter (fun x -> let (child, parent) = getParentChild x
                                tmpMap <- tmpMap.Add(child, parent) )
    tmpMap

let createOrbitLengthMap (childParentMap: Map<string,string>) : Map<string, int> =
    let mutable tmpMap = Map.empty

    let rec getOrbitalLength(obj: string) =
        let parent = childParentMap.TryFind(obj)
        if parent.IsNone then
            printfn "Obtaining parent for: %A" obj
            failwith "Parent not found for object"
        if parent.Value = "COM" then
            tmpMap <- tmpMap.Add(obj, 1)
            1
        else
            let parentLength = tmpMap.TryFind(parent.Value)
            if parentLength.IsNone then
                let newParentLength = getOrbitalLength(parent.Value)
                tmpMap <- tmpMap.Add(obj, newParentLength + 1)
                newParentLength + 1
            else
                tmpMap <- tmpMap.Add(obj, parentLength.Value + 1)
                parentLength.Value + 1

    childParentMap |> Map.iter (fun x _ -> getOrbitalLength(x) |> ignore)
    tmpMap


let Part1 = 
    let childParent = initChildParentMap
    let totalOrbitLengths = createOrbitLengthMap(childParent) |> Seq.sumBy (fun v -> v.Value)
    printfn "TotalNumOrbits: %d" totalOrbitLengths

let Part2 = 
    let childParent = initChildParentMap
    let orbitLengthMap = childParent |> createOrbitLengthMap

    let findOrbitalPathToCom(obj: string) = 
        let mutable path = List.empty
        let mutable currObj = obj
        while currObj <> "COM" do
            path <- List.append path [currObj]
            currObj <- childParent.TryFind(currObj).Value
        path
        
    let myPathToCom = findOrbitalPathToCom("YOU")
    let santaPathToCom = findOrbitalPathToCom("SAN")

    let closestCommonObj = myPathToCom |> List.find (fun obj -> santaPathToCom |> List.contains(obj))
    let closestCommonObjLength = orbitLengthMap.TryFind(closestCommonObj).Value
    let youSanDistance = (orbitLengthMap.TryFind("YOU").Value - closestCommonObjLength)
                         + (orbitLengthMap.TryFind("SAN").Value - closestCommonObjLength)
                         - 2

    printfn "%A" youSanDistance
