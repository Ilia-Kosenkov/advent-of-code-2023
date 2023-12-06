open System

let splitOptions =
    StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

type SpecificRange =
    { KeyStart: int
      ValueStart: int
      Length: int }

type Range =
    | SpecificRange of SpecificRange
    | AllRange

type Range with

    member this.Map(value: int) =
        match this with
        | SpecificRange { KeyStart = keyStart
                          ValueStart = valueStart
                          Length = length } ->
            if value >= keyStart && value < keyStart + length then
                Some(valueStart + value - keyStart)
            else
                None
        | AllRange -> Some(value)

type MapHeader = { From: string; To: string }


type Mapping =
    { Header: MapHeader; Items: Range list }

type Seeds = int array

type Input =
    | Seeds of Seeds
    | MapHeader of MapHeader
    | MapItem of SpecificRange


type ParsedInput =
    | Seeds of Seeds
    | Mapping of Mapping array


let parseInt (s: string) =
    let mutable result = 0

    match Int32.TryParse(s, &result) with
    | true -> Some result
    | false -> None

let seqFold items =
    items
    |> Seq.fold
        (fun (gameSets: 'a list option) (gameSet: 'a option) ->
            match (gameSets, gameSet) with
            | Some sets, Some set -> Some(List.append sets [ set ])
            | _ -> None)
        (Some [])


let seqFoldToArray items =
    items |> seqFold |> Option.map Array.ofList


let rec parseInput (lines: string seq) =
    seq {
        for line in lines do
            if line.StartsWith("seeds:") then
                yield
                    line.Substring(6).Split(' ', splitOptions)
                    |> Array.map parseInt
                    |> seqFoldToArray
                    |> Option.map Input.Seeds
            elif line.EndsWith("map:") then
                yield
                    match line.Substring(0, line.Length - 5).Split("-to-", splitOptions) with
                    | [| from; to_ |] -> Some(MapHeader { From = from; To = to_ })
                    | _ -> None
            else
                let numbers = line.Split(' ', splitOptions) |> Array.map parseInt |> seqFoldToArray

                yield
                    match numbers with
                    | Some(numbers) when numbers.Length = 3 ->
                        Some(
                            MapItem
                                { KeyStart = numbers[1]
                                  ValueStart = numbers[0]
                                  Length = numbers[2] }
                        )
                    | _ -> None
    }

let gatherMaps (input: Input list) =
    input
    |> Seq.fold
        (fun (maps: Mapping list option) (item: Input) ->
            match (maps, item) with
            | Some maps, MapHeader header -> Some({ Header = header; Items = [] } :: maps)
            | Some maps, MapItem item ->
                match maps with
                | head :: tail ->
                    Some(
                        { head with
                            Items = head.Items @ [ SpecificRange item ] }
                        :: tail
                    )
                | _ -> None
            | _ -> None)
        (Some([]))
    |> Option.map (
        List.map (fun map ->
            { map with
                Items = map.Items @ [ AllRange ] })
    )
    |> Option.map List.rev


let gatherInput (input: Input list) =
    match input with
    | Input.Seeds head :: tail ->
        match gatherMaps tail with
        | Some maps -> Some(head, maps)
        | _ -> None
    | _ -> None

let findSeeds ((seeds, maps): int array * Map<string, Mapping>) =
    let startName = "seed"
    let lastName = "location"

    seq {
        let mutable name = startName
        let mutable input = seeds

        while name <> lastName do
            let map = maps[name]

            input <-
                input
                |> Array.map (fun value -> (map.Items |> Seq.choose (fun range -> range.Map(value)) |> Seq.head))

            name <- map.Header.To

        input
    }



Environment.GetCommandLineArgs().[1].Split(Environment.NewLine, splitOptions)
|> parseInput
|> seqFold
|> Option.bind gatherInput
|> Option.map (fun (seeds, maps) -> (seeds, maps |> List.map (fun map -> (map.Header.From, map)) |> Map.ofList))
|> Option.map findSeeds
|> printfn "%A"
