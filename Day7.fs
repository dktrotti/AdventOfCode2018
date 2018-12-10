namespace AdventOfCode2018
open Common
open System

module Day7 =
    type Dependency = {Step:string; DependsOn:string seq}
    let parseDependency str =
        match str with
        | Regex "Step ([A-Z]) must.* before step ([A-Z])" [a; b] -> {Step=b; DependsOn=[a]}
        | _ -> raise <| ArgumentException("Invalid input: " + str)

    let groupDependencies (dependencies:Dependency seq) step =
        let dependencies' = dependencies
                            |> Seq.filter (fun d -> d.Step = step)
                            |> Seq.collect (fun d -> d.DependsOn)
        {Step=step; DependsOn=dependencies'}

    let allMatch (func: 'T -> bool) (keys: 'T seq) : bool =
        if Seq.isEmpty keys then
            true
        else keys
            |> Seq.map func
            |> Seq.reduce (&&)

    let isAvailable (rankMap: Map<string, int>) dependency =
        (allMatch rankMap.ContainsKey dependency.DependsOn)

    let rec orderDependencies (rankMap: Map<string, int>) currentRank (dependencies: Dependency list) =
        match dependencies with
        | [] -> rankMap
        | _ -> 
            let available, unavailable = List.partition (isAvailable rankMap) dependencies
            let available' = List.sortBy (fun (d:Dependency) -> d.Step) available
            let next = Seq.head available'
            orderDependencies (rankMap.Add(next.Step,currentRank)) (currentRank + 1) ((List.tail available') @ unavailable)

    let rankMapToList (rankMap: Map<string, int>) =
        Map.toList rankMap
            |> List.sortBy snd
            |> List.map fst

    let part1 filename =
        let dependencies = readLines filename
                            |> Seq.map parseDependency
        ['A'..'Z']
            |> List.map (string >> groupDependencies dependencies)
            |> orderDependencies Map.empty 0
            |> rankMapToList
            |> List.reduce (+)