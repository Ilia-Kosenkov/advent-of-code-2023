open System

// Iterate a `string` once, collecting first and last digit characters
// Returns either `Some(first, last)` or `None` if no digits found
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


let parseInt (c: char) = c |> string |> int

// Parse an optional pair of digit characters into an optional pair of numbers
let parseDigits pairOpt =
    pairOpt |> Option.map (fun (f, l) -> (parseInt f, parseInt l))

// Convert optional pair of ints into an optional int by combining digits
let constructNumber pairOpt =
    pairOpt |> Option.map (fun (f, l) -> f * 10 + l)

// Read input from stdin (new-line-separated string passed as first arg)
Environment
    .GetCommandLineArgs()
    .[1].Split(Environment.NewLine, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
|> Seq.map extractDigitCharacters // Extract digit characters
|> Seq.map parseDigits // Parse
|> Seq.map constructNumber // Construct check sums
|> Seq.fold // Compute sum if all check sums are Some, otherwise return None
    (fun sum lineCheckSum ->
        match (sum, lineCheckSum) with
        | (_, None) -> None
        | (None, _) -> None
        | (Some s, Some lcs) -> Some(s + lcs))
    (Some 0) // Start with 0. Is any of the check sums are None, accumulator is replaced with None
|> printfn "%A"
