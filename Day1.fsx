open System
open System.IO
let parseInt s =
    try
        int s
    with | exn ->
        printfn "Bad input %s" s
        0
let part1 filename =
    File.ReadAllLines filename
        |> Array.toList
        |> List.map parseInt
        |> List.sum
        |> printfn "Sum=%i"

type Frequency = {Value:int; Index:int}
[<CustomEquality; CustomComparison >]
type RepeatedFrequency =
    {
    Frequency:Frequency;
    Order:int
    }
    override this.Equals(other) =
        match other with
        | :? RepeatedFrequency as otherFreq -> 
            (compare this otherFreq) = 0
        | _ -> false

    interface IComparable with
        member this.CompareTo(other) =
            match other with
            | :? RepeatedFrequency as otherFreq ->
                if (compare this.Order otherFreq.Order) = 0
                then (compare this.Frequency.Index otherFreq.Frequency.Index)
                else (compare this.Order otherFreq.Order)
            | _ -> raise <| ArgumentException("Can't compare instances of different types")

let checkForRepeatingFrequency (sum:int) (value:Frequency) (other:Frequency) =
    let diff = value.Value - other.Value
    if (diff % sum = 0) && diff >= 0 then Some({Frequency={Value=value.Value; Index=other.Index}; Order=(diff/sum)})
    else None

let findRepeatingFrequencies (sum:int) (value:Frequency) (others:Frequency list) : RepeatedFrequency list =
    let check = checkForRepeatingFrequency sum value
    List.choose check others

let rec findAllRepeatingFrequencies (sum:int) (retval:RepeatedFrequency list) (processed:Frequency list) (unprocessed:Frequency list) : RepeatedFrequency list =
    let find = findRepeatingFrequencies sum
    match (processed, unprocessed) with
        | (_, []) -> retval
        | ([], first::rest) -> findAllRepeatingFrequencies sum (find first rest) [first] rest
        | (_, first::rest) -> (findAllRepeatingFrequencies sum (retval @ (find first (processed @ rest))) (processed @ [first]) rest)

let part2 filename =
    let nums = File.ReadAllLines filename
                |> Array.toList
                |> List.map parseInt
    let totalSum = List.sum nums
    let sums = (List.scan (+) 0 nums).[1..]
    let freqs = List.map2 (fun idx i -> {Value=i; Index=idx}) [0..(sums.Length-1)] sums
    findAllRepeatingFrequencies totalSum [] [] freqs
        |> List.min
        |> string
        |> printfn "First repeated frequency=%s"

part2 "data.txt"