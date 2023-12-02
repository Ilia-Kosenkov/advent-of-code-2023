open System
open System.Text.RegularExpressions
let digitRegex = Regex("\\d", RegexOptions.Compiled ||| RegexOptions.NonBacktracking)


// Iterate a `string` once, collecting first and last digit characters
// Returns either `Some(first, last)` or `None` if no digits found
let extractDigitCharacters line =
    let matches = line |> digitRegex.Matches
    Some ((matches |> Seq.head).Value, (matches |> Seq.last).Value)


let parseInt (matchedString: string) =
    match matchedString with
        | "1" -> Some 1
        | "2" -> Some 2
        | "3" -> Some 3
        | "4" -> Some 4
        | "5" -> Some 5
        | "6" -> Some 6
        | "7" -> Some 7
        | "8" -> Some 8
        | "9" -> Some 9
        | "0" -> Some 0
        | _ -> None

// Parse an optional pair of digit characters into an optional pair of numbers
let parseDigits pairOpt =
    match (pairOpt |> Option.map (fun (f, l) -> (parseInt f, parseInt l))) with
    | Some(Some first, Some last) -> Some((first, last))
    | _ -> None

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
        | Some s, Some lcs -> Some(s + lcs))
    (Some 0) // Start with 0. Is any of the check sums are None, accumulator is replaced with None
|> printfn "%A"
