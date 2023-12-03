open System

let splitOptions =
    StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

type DigitPosition =
    | Start of int
    | End of int

type NumberPosition =
    { Start: int
      End: int }

    member this.OverlapsWith(position: int) =
        (this.Start - 1) <= position && position <= this.End


type SymbolPosition = { Symbol: char; Position: int }

let seqFold items =
    items
    |> Seq.fold
        (fun (gameSets: 'a list option) (gameSet: 'a option) ->
            match (gameSets, gameSet) with
            | Some sets, Some set -> Some(List.append sets [ set ])
            | _ -> None)
        (Some [])

let getSymbolPositions (line: string) =
    line
    |> Seq.indexed
    |> Seq.choose (fun (i, c) ->
        if Char.IsDigit(c) || c = '.' then
            None
        else
            Some({ Symbol = c; Position = i }))
    |> Array.ofSeq

let prependAppend (prepend: 'a) (append: 'a) (items: 'a seq) =
    seq {
        yield prepend
        yield! items
        yield append
    }

let getNumberPositions (line: string) =
    line
    |> prependAppend '.' '.'
    |> Seq.windowed 2
    |> Seq.indexed
    |> Seq.choose (fun (i, w) ->
        if Char.IsDigit(w[0]) && not (Char.IsDigit(w[1])) then
            Some(End(i))
        elif not (Char.IsDigit(w[0])) && Char.IsDigit(w[1]) then
            Some(Start(i))
        else
            None)
    |> Seq.chunkBySize 2
    |> Seq.map (fun w ->
        match w with
        | [| Start(s); End(e) |] -> Some({ Start = s; End = e })
        | _ -> None)
    |> seqFold
    |> Option.map Array.ofSeq

type LineInfo =
    { Line: string
      Symbols: SymbolPosition array
      Numbers: NumberPosition array }


let processLine line =
    let symbolPositions = getSymbolPositions line
    let numberPositions = getNumberPositions line

    match numberPositions with
    | Some(np) ->
        Some(
            { Line = line
              Symbols = symbolPositions
              Numbers = np }
        )
    | _ -> None

let seqAny sequence = sequence |> Seq.isEmpty |> not

let countAdjacentNumbersHorizontal (lineInfo: LineInfo) (symbol: SymbolPosition) =
    let hasOnTheLeft =
        lineInfo.Numbers |> Seq.where (fun n -> n.End = symbol.Position) |> seqAny

    let hasOnTheRight =
        lineInfo.Numbers |> Seq.where (fun n -> n.Start = symbol.Position + 1) |> seqAny

    match (hasOnTheLeft, hasOnTheRight) with
    | (true, true) -> 2
    | (false, false) -> 0
    | _ -> 1

let countAdjacentNumbersVertical (lineInfo: LineInfo) (symbol: SymbolPosition) =
    lineInfo.Numbers
    |> Seq.where (fun n -> n.OverlapsWith(symbol.Position))
    |> Seq.length

let rec findGears (input: LineInfo array) =
    let previous = input[0]
    let current = input[1]
    let next = input[2]

    let gearIndices = current.Symbols |> Seq.where (fun s -> s.Symbol = '*')

    let partsCount =
        gearIndices
        |> Seq.map (fun g ->
            (g,
             (countAdjacentNumbersHorizontal current g)
             + (countAdjacentNumbersVertical previous g)
             + (countAdjacentNumbersVertical next g)))


    (current, partsCount)

let parseInt (s: ReadOnlySpan<char>) =
    let mutable result = 0

    match Int32.TryParse(s, &result) with
    | true -> Some result
    | false -> None


Environment.GetCommandLineArgs().[1].Split(Environment.NewLine, splitOptions)
|> Array.map processLine
|> seqFold
|> Option.map (fun lineInfo ->
    { Line = ""
      Symbols = [||]
      Numbers = [||] }
    :: lineInfo
    @ [ { Line = ""
          Symbols = [||]
          Numbers = [||] } ])
|> Option.map (Seq.windowed 3)
|> Option.map (Seq.map findGears)
|> Option.map Array.ofSeq
|> printfn "%A"
