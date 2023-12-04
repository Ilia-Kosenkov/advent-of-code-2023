open System
open Microsoft.FSharp.Collections

let splitOptions =
    StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

type Card =
    { Id: int
      WinningNumbers: int array
      CardNumbers: int array }

    member this.GetNextCardIds() =
        let scores =
            this.WinningNumbers
            |> Set.ofArray
            |> Set.intersect (this.CardNumbers |> Set.ofArray)
            |> Seq.length

        seq {
            for i in 1..scores do
                yield this.Id + i
        }

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


type CardSet = { Id: int; NextCards: int array }

let mergeSets (old: CardSet) (newItem: CardSet) =
    let count = old.NextCards |> Seq.where (fun i -> i = newItem.Id) |> Seq.length

    let newCards =
        Seq.replicate (count + 1) (seq { yield! newItem.NextCards }) |> Seq.concat

    { Id = 0
      NextCards = [| yield! old.NextCards; yield newItem.Id; yield! newCards |] }

Environment.GetCommandLineArgs().[1].Split(Environment.NewLine, splitOptions)
|> Seq.map parseCard
|> seqFold
|> Option.map (
    Seq.map (fun card ->
        { Id = card.Id
          NextCards = card.GetNextCardIds() |> Array.ofSeq })
)
|> Option.map Array.ofSeq
|> Option.map (Seq.fold mergeSets { Id = 0; NextCards = [||] })
|> Option.map (fun set -> set.NextCards.Length)
// |> Option.map (fun set ->
//     (set.NextCards |> Array.groupBy id |> Array.map (fun (x, s) -> (x, s.Length)), set.NextCards.Length))
|> printfn "%A"
