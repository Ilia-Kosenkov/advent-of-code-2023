open System

let splitOptions =
    StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

type DigitPosition =
    | Start of int
    | End of int

type NumberPosition = { Start: int; End: int }

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
    |> Seq.choose (fun (i, c) -> if Char.IsDigit(c) || c = '.' then None else Some(i))
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
      Symbols: int array
      Numbers: NumberPosition array }


let processLine line =
    let symbolPositions = getSymbolPositions line
    let numberPositions = getNumberPositions line
    numberPositions |> printfn "%A"

    match numberPositions with
    | Some(np) ->
        Some(
            { Line = line
              Symbols = symbolPositions
              Numbers = np }
        )
    | _ -> None

let hasHorizontalSymbols (input: LineInfo) =
    input.Numbers
    |> Seq.where (fun number ->
        (input.Symbols |> Array.contains (number.Start - 1))
        || input.Symbols |> Array.contains number.End)


let hasVerticalSymbols (symbols: int array) (input: LineInfo) =
    input.Numbers
    |> Seq.where (fun number ->
        symbols
        |> Array.tryFind (fun symb -> symb >= number.Start - 1 && symb <= number.End)
        |> Option.isSome)

type Result =
    { Line: string
      PartNumbers: NumberPosition array }

let rec findPartNumbers (input: LineInfo array) =
    let previous = input[0]
    let current = input[1]
    let next = input[2]

    let identifiedByHorizontalSymbols = current |> hasHorizontalSymbols |> Array.ofSeq

    let identifiedFromTop =
        current |> (hasVerticalSymbols previous.Symbols) |> Array.ofSeq

    let identifiedFromBottom =
        current |> (hasVerticalSymbols next.Symbols) |> Array.ofSeq

    let partNumbers =
        seq {
            yield identifiedFromTop
            yield identifiedByHorizontalSymbols
            yield identifiedFromBottom
        }
        |> Seq.concat
        |> Seq.distinct
        |> Array.ofSeq

    { Line = current.Line
      PartNumbers = partNumbers }

let parseInt (s: ReadOnlySpan<char>) =
    let mutable result = 0

    match Int32.TryParse(s, &result) with
    | true -> Some result
    | false -> None

let parsePartNumbers (input: Result) =
    input.PartNumbers
    |> Seq.map (fun number -> parseInt (input.Line.AsSpan(number.Start, number.End - number.Start)))
    |> seqFold
    |> Option.map Array.ofSeq

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
|> Option.map (Seq.map findPartNumbers)
|> Option.map (Seq.map parsePartNumbers)
|> printfn "%A"
