namespace AdventOfCode2018
open Common
open System

module Day6 =
    type Location = {Id:int; X:int; Y:int}
    let parseLocation id str =
        match str with
        | Regex "([0-9]+), ([0-9]+)" [x; y] -> {Id=id; X=int x; Y=int y}
        | _ -> raise <| ArgumentException("Invalid input: " + str)

    let manhattanDistance x y location : int =
        abs (location.X - x) + abs (location.Y - y)

    let findNearest (locations: Location seq) x y =
        printfn "x=%i y=%i" x y
        Seq.groupBy (manhattanDistance x y) locations
            |> Seq.minBy fst
            |> fun (dist, locs) ->
                if Seq.length locs = 1
                    then Some (Seq.head locs)
                    else None

    let part1 filename =
        let locations = readLines filename
                        |> Seq.mapi parseLocation

        let areas = Array2D.init 400 400 (findNearest locations)
        let borders = seq {
            for x in 0..399 do
                if x = 0 || x = 399
                    then
                        for y in 0..399 do
                            yield (x,y)
                    else
                        yield (x,0)
                        yield (x,399)                        
        }
        let infinites = borders
                        |> Seq.choose (fun (x,y) -> areas.[x, y])
                        |> Seq.distinct
                        |> Seq.toList

        Seq.cast<Location option> areas
            |> Seq.choose id
            |> Seq.countBy id
            |> Seq.filter (fun (loc,count) -> not (Seq.contains loc infinites))
            |> Seq.map snd
            |> Seq.max


    let sumDistances locations x y =
        printfn "x=%i y=%i" x y
        locations
            |> Seq.map (manhattanDistance x y)
            |> Seq.sum

    let part2 filename =
        let locations = readLines filename
                        |> Seq.mapi parseLocation
        Array2D.init 400 400 (sumDistances locations)
            |> Seq.cast<int>
            |> Seq.filter (fun i -> i < 10000)
            |> Seq.length