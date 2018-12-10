namespace AdventOfCode2018
open Common
open System

module Day7 =
    type Dependency = {Step:char; DependsOn:char seq}
    let parseDependency str =
        match str with
        | Regex "Step ([A-Z]) must.* before step ([A-Z])" [a; b] -> {Step=b.Chars 0; DependsOn=[a.Chars 0]}
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

    let isAvailable (rankMap: Map<char, int>) dependency =
        (allMatch rankMap.ContainsKey dependency.DependsOn)

    let rec orderDependencies (rankMap: Map<char, int>) currentRank (dependencies: Dependency list) =
        match dependencies with
        | [] -> rankMap
        | _ -> 
            let available, unavailable = List.partition (isAvailable rankMap) dependencies
            let available' = List.sortBy (fun (d:Dependency) -> d.Step) available
            let next = Seq.head available'
            orderDependencies (rankMap.Add(next.Step,currentRank)) (currentRank + 1) ((List.tail available') @ unavailable)

    let rankMapToList (rankMap: Map<char, int>) =
        Map.toList rankMap
            |> List.sortBy snd
            |> List.map fst

    let part1 filename =
        let dependencies = readLines filename
                            |> Seq.map parseDependency
        ['A'..'Z']
            |> List.map (groupDependencies dependencies)
            |> orderDependencies Map.empty 0
            |> rankMapToList
            |> List.reduce (+)


    type FinishedTask = {Step:char; Time:int}

    let isComplete currentTime finishedTask =
        finishedTask.Time <= currentTime

    let isAvailable2 (completed: FinishedTask list) dependency =
        let steps = Seq.map (fun t -> t.Step) completed
        allMatch (fun s -> Seq.contains s steps) dependency.DependsOn

    let getTime c =
        // A = 65, so subtract 64 to get correct value
        (int c) - 64 + 60

    let finishTask time (dependency:Dependency) =
        {Step=dependency.Step; Time=time + (getTime dependency.Step)}

    let safeSkip count list =
        if count > (List.length list) then
            []
        else
            List.skip count list

    let rec orderTimedDependencies currentTime (completed: FinishedTask list) (inProgress: FinishedTask list) (dependencies: Dependency list) =
        let newlyCompleted, inProgress' = List.partition (isComplete currentTime) inProgress
        let completed' = completed @ newlyCompleted
        match Seq.length inProgress', Seq.length dependencies  with
        | _, 0 -> completed' @ inProgress'
        | 5, _ -> orderTimedDependencies (currentTime + 1) completed' inProgress' dependencies
        | _ -> 
            let available, unavailable = List.partition (isAvailable2 completed') dependencies
            let available' = List.sortBy (fun (d:Dependency) -> d.Step) available
            let workerCount = 5 - (Seq.length inProgress')
            let tasks = List.truncate workerCount available'
                        |> List.map (finishTask currentTime)
            orderTimedDependencies (currentTime + 1) completed' (tasks @ inProgress') ((safeSkip workerCount available') @ unavailable)

    let part2 filename =
        let dependencies = readLines filename
                            |> Seq.map parseDependency
        ['A'..'Z']
            |> List.map (groupDependencies dependencies)
            |> orderTimedDependencies 0 [] []
            |> List.maxBy (fun t -> t.Time)
