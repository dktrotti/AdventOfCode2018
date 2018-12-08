namespace AdventOfCode2018
open System.IO
open System.Text.RegularExpressions

module Common =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let (=?) s1 s2 =
        System.String.Equals(s1, s2, System.StringComparison.CurrentCultureIgnoreCase)    

    let mapToTuple (func: 'T->'U) (sequence: 'T seq) : ('T*'U) seq =
        Seq.map (fun t -> (t, func t)) sequence

    let mapFirst (func: 'T->'V) (sequence: ('T*'U) seq) : ('V*'U) seq =
        Seq.map (fun (first, second) -> (func first, second)) sequence

    let mapSecond (func: 'U->'V) (sequence: ('T*'U) seq) : ('T*'V) seq =
        Seq.map (fun (first, second) -> (first, func second)) sequence