namespace AdventOfCode2018
open Common
open System

module Day5 =
    let isReactive (c1:char) (c2:char) =
        Char.ToUpper c1 = Char.ToUpper c2 && (Char.IsUpper c1 <> Char.IsUpper c2)

    let rec reactAt index (polymer:string) =
        if (0 <= index && index < polymer.Length - 1) && (isReactive polymer.[index] polymer.[index + 1])
            then reactAt (index - 1) (polymer.Remove(index, 2))
            else polymer          

    let rec reducePolymer (polymer:string) =
        match Seq.length polymer with
        | 0 | 1 -> polymer
        | _ -> 
            let split = polymer.Length / 2
            let first = reducePolymer (polymer.Substring(0, split))
            let second = reducePolymer (polymer.Substring(split))
            reactAt (first.Length - 1) (first + second)

    let part1 filename =
        readLines filename
            |> Seq.head
            |> reducePolymer
            |> Seq.length
            |> printfn "Reacted length=%i"
    
    let reducePolymerWithout (polymer:string) c =
        reducePolymer (polymer.Replace(c.ToString(), "").Replace((Char.ToUpper c).ToString(), ""))

    let part2 filename =
        let polymer = Seq.head (readLines filename)
        ['a'..'z']
            |> mapToTuple (reducePolymerWithout polymer)
            |> mapSecond Seq.length
            |> Seq.minBy snd
            |> (fun (c, len) -> printfn "Worst character=%c, length=%i" c len)