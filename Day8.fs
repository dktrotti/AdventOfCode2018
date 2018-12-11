namespace AdventOfCode2018
open Common
open System

module Day8 =
    type Node = {Children: Node list; Metadata: int list}

    let rec parseNode siblingCount (data: int list) : ((Node list) * (int list)) =
        let childCount::metadataSize::rest = data
        let (children, remaining) = if (childCount <> 0) then
                                        parseNode childCount rest
                                    else
                                        ([], rest)
        let node = {Children=children; Metadata=List.take metadataSize remaining}
        let remaining' = List.skip metadataSize remaining
        match siblingCount - 1 with
        | 0 ->
            ([node], remaining')
        | _ ->
            let (siblings, remaining'') = parseNode (siblingCount - 1) remaining'
            (node::siblings, remaining'')

    let rec sumMetadata nodes =
        match nodes with
        | [] -> 0
        | first::rest -> (Seq.sum first.Metadata) + (sumMetadata first.Children) + (sumMetadata rest)

    let part1 filename =
        readLines filename
            |> Seq.head
            |> fun s -> s.Split([|' '|])
            |> Seq.map int
            |> Seq.toList
            |> parseNode 1
            |> fst
            |> sumMetadata

    let tryGet (arr: Node array) idx : Node option =
        if idx < (Array.length arr) then Some arr.[idx] else None

    let rec getNodeValue node =
        match node.Children with
        | [] -> Seq.sum node.Metadata
        | _ ->
            let children = Seq.toArray node.Children
            node.Metadata
                |> Seq.filter (fun idx -> idx <> 0)
                |> Seq.choose (fun idx -> tryGet children (idx - 1))
                |> Seq.map getNodeValue
                |> Seq.sum

    let part2 filename =
        readLines filename
            |> Seq.head
            |> fun s -> s.Split([|' '|])
            |> Seq.map int
            |> Seq.toList
            |> parseNode 1
            |> fst
            |> Seq.head
            |> getNodeValue