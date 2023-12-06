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

type InputMap =
    { From: string
      To: String
      Map: Range array }

type Input =
    | Seeds of int array
    | InputMap of InputMap

let parseInt (s: ReadOnlySpan<char>) =
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
                    |> Array.map (fun num -> parseInt (num.AsSpan()))
                    |> seqFoldToArray
                    |> Option.map Seeds

    }


Environment.GetCommandLineArgs().[1].Split(Environment.NewLine, splitOptions)
|> parseInput
|> printfn "%A"
