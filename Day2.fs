namespace AdventOfCode2018
open System
open Common

module Day2 =
    let getDistinctLetterCounts str =
        Seq.countBy id str
            |> Seq.map (fun (letter, count) -> count)
            |> Seq.distinct

    let part1 filename =
        readLines filename
            |> Seq.collect getDistinctLetterCounts
            |> Seq.filter (fun count -> count=2 || count=3)
            |> Seq.countBy id
            |> Seq.map (fun (charCount, count) -> count)
            |> Seq.reduce (*)
            |> printfn "Checksum=%i"

    let hammingDistance str1 str2 : int =
        Seq.zip str1 str2
            |> Seq.sumBy (fun (c1, c2) -> if c1=c2 then 0 else 1)

    let rec checkForHammingDistanceOf1 str others : (string * string) option =
        match others with
        | first::rest -> 
            let dist = hammingDistance str first
            if dist=1
            then Some (str, first)
            else checkForHammingDistanceOf1 str rest
        | [] -> None

    let rec findHammingDistanceOf1 strings : (string * string) =
        match strings with
        | first::rest ->
            let pair = checkForHammingDistanceOf1 first rest
            match pair with
            | Some (x, y) -> (x, y)
            | None -> findHammingDistanceOf1 rest
        | [] -> raise <| ArgumentException("Can't find any strings with Hamming distance of 1")

    let part2 filename =
        readLines filename
        |> Seq.toList
        |> findHammingDistanceOf1
        |> string
        |> printfn "Pair=%s"

    part2 "data.txt"