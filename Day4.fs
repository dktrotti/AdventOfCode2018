namespace AdventOfCode2018
open Common
open System.Text.RegularExpressions
open System

module Day4 =
    // Source: https://stackoverflow.com/a/2281901
    // Splits a list into two lists using the specified function
    // The list is split between two elements for which 'f' returns 'true'
    let splitAt f list =
      let rec splitAtAux acc list = 
        match list with
        | x::y::ys when f x y -> List.rev (x::acc), y::ys
        | x::xs -> splitAtAux (x::acc) xs
        | [] -> (List.rev acc), []
      splitAtAux [] list

    // Repeatedly uses 'f' to take several elements of the input list and
    // aggregate them into value of type 'b until the remaining list 
    // (second value returned by 'f') is empty
    let foldUntilEmpty f list = 
      let rec foldUntilEmptyAux acc list =
        match f list with
        | l, [] -> l::acc |> List.rev
        | l, rest -> foldUntilEmptyAux (l::acc) rest
      foldUntilEmptyAux [] list

    let splitAtEvery f list = foldUntilEmpty (splitAt f) list

    type Nap = {Start:int; End:int}
    type GuardDuty = {Id:int; Naps:Nap seq; Date:DateTime;}
    type GuardHistory = {Id:int; Duties:GuardDuty seq}

    let parseId guardText =
        let regexMatch = Regex.Match(guardText, ".*Guard #([0-9]+)")
        match regexMatch.Success with
        | true -> int regexMatch.Groups.[1].Value
        | false -> raise <| ArgumentException("Invalid input: " + guardText)

    let parseNap pair =
        match pair with
        | (Regex ".*:([0-9]+)\] falls asleep" [asleepTime], Regex ".*:([0-9]+)\] wakes up" [wakeTime]) ->
            Some {Start=int asleepTime; End=int wakeTime}
        | _ -> None

    let parseDate line = 
        match line with
        | Regex "\[(.*)\]" [date] -> DateTime.Parse(date)
        | _ -> raise <| ArgumentException("Could not parse date: " + line)

    let parseNaps napTexts =
        Seq.pairwise napTexts
        |> Seq.choose parseNap

    let parseGuardDuty strs =
        let guard::naps = strs
        {Id=parseId guard; Naps=parseNaps naps; Date=((parseDate guard).AddHours(1.0))}

    let sumNaps history =
        history.Duties
            |> Seq.map (fun duty -> duty.Naps)
            |> Seq.concat
            |> Seq.sumBy (fun nap -> nap.End - nap.Start)

    let isInNap minute nap =
        nap.Start <= minute && minute < nap.End

    let napCount minute naps =
        Seq.map (isInNap minute) naps
            |> Seq.filter id
            |> Seq.length

    type SleepyMinute = {Minute:int; Count:int}
    let findSleepiestMinute history =
        let naps = history.Duties
                    |> Seq.map (fun duty -> duty.Naps)
                    |> Seq.concat
        [0..59]
            |> Seq.map (fun minute -> {Minute=minute; Count=napCount minute naps})
            |> Seq.maxBy (fun m -> m.Count )

    let part1 filename =
        readLines filename
            |> Seq.sort
            |> Seq.toList
            |> splitAtEvery (fun s1 s2 -> s2.Contains "Guard")
            |> Seq.map parseGuardDuty
            |> Seq.groupBy (fun g -> g.Id)
            |> Seq.map (fun (id, duties) -> {Id=id; Duties=duties})
            |> Seq.maxBy sumNaps
            |> (fun history -> printfn "Laziest guard=%i; Sleepiest minute=%i" history.Id (findSleepiestMinute history).Minute)

    let part2 filename =
        readLines filename      
            |> Seq.sort
            |> Seq.toList
            |> splitAtEvery (fun s1 s2 -> s2.Contains "Guard")
            |> Seq.map parseGuardDuty
            |> Seq.groupBy (fun g -> g.Id)
            |> Seq.map (fun (id, duties) -> {Id=id; Duties=duties})
            |> Seq.map (fun history -> (history.Id, findSleepiestMinute history))
            |> Seq.maxBy (fun (_,m) -> m.Count )
            |> fun (id, minute) -> printfn "Laziest guard=%i; Sleepiest minute=%i" id minute.Minute

    part2 "data.txt"        