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

type MapHeader = { From: string; To: string }

type Input =
    | Seeds of int array
    | MapHeader of MapHeader
    | MapItem of SpecificRange

let parseInt (s: string) =
    let mutable result = 0

    match Int32.TryParse(s, &result) with
    | true -> Some result
    | false -> None

let seqFoldToArray items =
    items
    |> Seq.fold
        (fun (gameSets: 'a list option) (gameSet: 'a option) ->
            match (gameSets, gameSet) with
            | Some sets, Some set -> Some(List.append sets [ set ])
            | _ -> None)
        (Some [])
    |> Option.map Array.ofList


let rec parseInput (lines: string seq) =
    seq {
        for line in lines do
            if line.StartsWith("seeds:") then
                yield
                    line.Substring(6).Split(' ', splitOptions)
                    |> Array.map parseInt
                    |> seqFoldToArray
                    |> Option.map Seeds
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
                                { KeyStart = numbers[0]
                                  ValueStart = numbers[1]
                                  Length = numbers[2] }
                        )
                    | _ -> None
    }


Environment.GetCommandLineArgs().[1].Split(Environment.NewLine, splitOptions)
|> parseInput
|> printfn "%A"
