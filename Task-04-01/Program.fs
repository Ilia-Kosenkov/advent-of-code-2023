open System

let splitOptions =
    StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

type Card =
    { Id: int
      WinningNumbers: int array
      CardNumbers: int array }

    member this.GetScores() =
        this.WinningNumbers
        |> Set.ofArray
        |> Set.intersect (this.CardNumbers |> Set.ofArray)

let parseInt (s: ReadOnlySpan<char>) =
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


let parseIndex (colonIdx: int) (line: string) =
    if colonIdx <= 0 then
        None
    else
        parseInt (line.AsSpan(4, colonIdx - 4))

let parseNumbers (startFrom: int) (readTo: int) (line: string) =
    line[startFrom..readTo].Split(' ', splitOptions)
    |> Seq.map (fun s -> parseInt (s.AsSpan()))
    |> seqFold
    |> Option.map Array.ofList

let parseCard (line: string) =
    let colonIdx = line.IndexOf(':')
    let barIdx = line.IndexOf('|')
    let cardIdx = line |> parseIndex colonIdx

    cardIdx
    |> Option.map (fun id ->
        { Id = id
          WinningNumbers = [||]
          CardNumbers = [||] })
    |> Option.bind (fun card ->
        parseNumbers (colonIdx + 1) (barIdx - 1) line
        |> Option.map (fun winningNumbers ->
            { card with
                WinningNumbers = winningNumbers }))
    |> Option.bind (fun card ->
        parseNumbers (barIdx + 1) line.Length line
        |> Option.map (fun cardNumbers -> { card with CardNumbers = cardNumbers }))



Environment.GetCommandLineArgs().[1].Split(Environment.NewLine, splitOptions)
|> Seq.map parseCard
|> seqFold
|> Option.map (List.map (fun card -> card.GetScores()))
|> Option.map (List.map (fun scores -> pown 2 (scores.Count - 1)))
|> Option.map List.sum
|> printfn "%A"
