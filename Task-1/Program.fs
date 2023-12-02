open System

let extractDigitCharacters line =
    match
        (line
         |> Seq.where Char.IsAsciiDigit
         |> Seq.fold
             (fun (first, _) c ->
                 match first with
                 | Some f -> (Some f, Some c)
                 | _ -> (Some c, Some c))
             (None, None))
    with
    | Some f, Some l -> Some(f, l)
    | _ -> None


let parseDigits pairOpt =
    match pairOpt with
    | Some(f, l) -> Some(f |> string |> int, l |> string |> int)
    | _ -> None

let constructNumber pairOpt =
    match pairOpt with
    | Some(f, l) -> Some(f * 10 + l)
    | _ -> None

Environment.GetCommandLineArgs().[1].Split(Environment.NewLine)
|> Seq.map extractDigitCharacters
|> Seq.map parseDigits
|> Seq.map constructNumber
|> Seq.fold
    (fun sum number ->
        match (sum, number) with
        | (_, None) -> None
        | (None, _) -> None
        | (Some s, Some n) -> Some(s + n))
    (Some 0)
|> printfn "%A"
