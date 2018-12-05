namespace AdventOfCode2018
open System.Text.RegularExpressions

module Day3 =
    type Claim = {Id:int; PosX:int; PosY:int; SizeX:int; SizeY:int}
    let emptyClaim = {Id=(-1); PosX=0; PosY=0; SizeX=0; SizeY=0}

    let parseClaim (str:string) =
        let regexMatch = Regex.Match(str, "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)")
        match regexMatch.Success with
        | true ->
            Some {
                Id=int regexMatch.Groups.[1].Value;
                PosX=int regexMatch.Groups.[2].Value;
                PosY=int regexMatch.Groups.[3].Value;
                SizeX=int regexMatch.Groups.[4].Value;
                SizeY=int regexMatch.Groups.[5].Value
            }
        | false ->
            printfn "Invalid input: %s" str
            None

    let isInClaim claim x y : bool =
        claim.PosX <= x && x < (claim.PosX + claim.SizeX) &&
            claim.PosY <= y && y < (claim.PosY + claim.SizeY)

    let rec isContested claims claimed x y : bool =
        match claims with
        | first::rest ->
            match (claimed, isInClaim first x y) with
            | true, true -> true
            | false, true -> isContested rest true x y 
            | _, false -> isContested rest claimed x y
        | [] -> false

    let part1 filename =
        let claims = (Common.readLines filename
            |> Seq.choose parseClaim
            |> Seq.toList)

        seq {
            for x in 0..1000 do
                for y in 0..1000 do
                    yield (x,y)
        }
        |> Seq.map (fun (x,y) -> isContested claims false x y)
        |> Seq.filter id
        |> Seq.length
        |> printfn "Overlapping squares=%i"

    let valuesOverlap min1 max1 min2 max2 : bool =
        (min2 <= min1 && min1 < max2) ||
            (min1 <= min2 && min2 < max1)

    let claimsOverlap claim1 claim2 : bool =
        valuesOverlap claim1.PosX (claim1.PosX + claim1.SizeX) claim2.PosX (claim2.PosX + claim2.SizeX) &&
            valuesOverlap claim1.PosY (claim1.PosY + claim1.SizeY) claim2.PosY (claim2.PosY + claim2.SizeY)

    let rec anyOverlap claim others : bool =
        match others with
        | first::rest -> 
            if (claimsOverlap claim first)
            then true
            else anyOverlap claim rest
        | [] -> false

    let rec findNoOverlap (processed:Claim list) (unprocessed:Claim list) : Claim option =
        match (unprocessed) with
        | first::rest ->
            if (anyOverlap first (processed@rest))
            then findNoOverlap (first::processed) rest
            else Some first
        | [] -> None

    let part2 filename =
        Common.readLines filename
            |> Seq.choose parseClaim
            |> Seq.toList
            |> findNoOverlap []
            |> string
            |> printfn "Intact claim=%s"

    part1 "data.txt"    