open System

let hasEnough required recorded =
    match recorded with
    | Some has -> has <= required
    | None -> true

type GameSet =
    { RedCount: int option
      GreenCount: int option
      BlueCount: int option }

    member this.HasEnoughRed(required: int) = hasEnough required this.RedCount

    member this.HasEnoughGreen(required: int) = hasEnough required this.GreenCount

    member this.HasEnoughBlue(required: int) = hasEnough required this.BlueCount

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

let parseGame (line: string) =
    let indexOfColon = line.IndexOf(':')

    if indexOfColon <= 0 then
        None
    else
        let gameId = parseInt (line.AsSpan(4, indexOfColon - 4))

        let gameSets =
            line.Substring(indexOfColon + 1).Split(';', splitOptions)
            |> Seq.map parseGameSet
            |> Seq.fold
                (fun (gameSets: GameSet list option) (gameSet: GameSet option) ->
                    match (gameSets, gameSet) with
                    | (Some sets, Some set) -> Some(List.append sets [ set ])
                    | _ -> None)
                (Some [])

        match (gameId, gameSets) with
        | (Some id, Some sets) ->
            Some
                { Id = id
                  GameSets = sets |> Array.ofList }
        | _ -> None

let matchGame (game: Game) (requiredRed: int) (requiredGreen: int) (requiredBlue: int) =
    if
        Array.TrueForAll(
            game.GameSets,
            (fun gameSet ->
                (gameSet.HasEnoughRed requiredRed)
                && (gameSet.HasEnoughGreen requiredGreen)
                && (gameSet.HasEnoughBlue requiredBlue))
        )
    then
        Some(game.Id)
    else
        None

Environment.GetCommandLineArgs().[1].Split(Environment.NewLine, splitOptions)
|> Seq.fold
    (fun list line ->
        match (list, parseGame line) with
        | (Some list, Some game) -> Some(List.append list [ game ])
        | _ -> None)
    (Some([]))
|> Option.map (List.choose (fun game -> matchGame game 12 13 14))
|> printfn "%A"
