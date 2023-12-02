open System

type GameSet =
    { RedCount: int option
      GreenCount: int option
      BlueCount: int option }

type Game = { Id: int; GameSets: GameSet array }

let parseInt (s: ReadOnlySpan<char>) =
    let mutable result = 0

    match Int32.TryParse(s, &result) with
    | true -> Some result
    | false -> None

let splitOptions =
    StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

let parseGameSet (s: string) =
    s.Split(',', splitOptions)
    |> Array.fold
        (fun (gameSet: GameSet option) (record: string) ->
            gameSet
            |> Option.bind (fun gameSet ->
                if record.EndsWith("red") then
                    match gameSet.RedCount with
                    | Some _ -> None
                    | None ->
                        Some
                            { gameSet with
                                RedCount = parseInt (record.AsSpan(0, record.Length - 3)) }
                elif record.EndsWith("green") then
                    match gameSet.GreenCount with
                    | Some _ -> None
                    | None ->
                        Some
                            { gameSet with
                                GreenCount = parseInt (record.AsSpan(0, record.Length - 5)) }
                elif record.EndsWith("blue") then
                    match gameSet.BlueCount with
                    | Some _ -> None
                    | None ->
                        Some
                            { gameSet with
                                BlueCount = parseInt (record.AsSpan(0, record.Length - 4)) }
                else
                    None))
        (Some(
            { RedCount = None
              GreenCount = None
              BlueCount = None }
        ))

let seqFold items =
    items
    |> Seq.fold
        (fun (gameSets: 'a list option) (gameSet: 'a option) ->
            match (gameSets, gameSet) with
            | Some sets, Some set -> Some(List.append sets [ set ])
            | _ -> None)
        (Some [])

let parseGame (line: string) =
    let indexOfColon = line.IndexOf(':')

    if indexOfColon <= 0 then
        None
    else
        let gameId = parseInt (line.AsSpan(4, indexOfColon - 4))

        let gameSets =
            line.Substring(indexOfColon + 1).Split(';', splitOptions)
            |> Seq.map parseGameSet
            |> seqFold

        match (gameId, gameSets) with
        | Some id, Some sets ->
            Some
                { Id = id
                  GameSets = sets |> Array.ofList }
        | _ -> None

let findMaxCubes game =
    let redCount = game.GameSets |> Seq.choose (fun set -> set.RedCount) |> Seq.max
    let greenCount = game.GameSets |> Seq.choose (fun set -> set.GreenCount) |> Seq.max
    let blueCount = game.GameSets |> Seq.choose (fun set -> set.BlueCount) |> Seq.max

    redCount * greenCount * blueCount

Environment.GetCommandLineArgs().[1].Split(Environment.NewLine, splitOptions)
|> Seq.map parseGame
|> seqFold
|> Option.map (Seq.map findMaxCubes)
|> Option.map Seq.sum
|> printfn "%A"
