open System
open System.Text.RegularExpressions
let digitRegex = Regex("\\d|one|two|three|four|five|six|seven|eight|nine", RegexOptions.Compiled ||| RegexOptions.NonBacktracking)


let extractDigitCharacters line =
    let matches = line |> digitRegex.Matches
    match matches.Count with
    | 0 -> None
    | n -> Some(matches[0].Value, matches[n-1].Value)


let parseInt (matchedString: string) =
    match matchedString with
        | "1" | "one" -> Some 1
        | "2" | "two" -> Some 2
        | "3" | "three" -> Some 3
        | "4" | "four" -> Some 4
        | "5" | "five" -> Some 5
        | "6" | "six" -> Some 6
        | "7" | "seven" -> Some 7
        | "8" | "eight" -> Some 8
        | "9" | "nine" -> Some 9
        | "0" -> Some 0
        | _ -> None

let parseDigits pairOpt =
    match (pairOpt |> Option.map (fun (f, l) -> (parseInt f, parseInt l))) with
    | Some(Some first, Some last) -> Some((first, last))
    | _ -> None

let constructNumber pairOpt =
    pairOpt |> Option.map (fun (f, l) -> f * 10 + l)

// Read input from stdin (new-line-separated string passed as first arg)
Environment
    .GetCommandLineArgs()
    .[1].Split(Environment.NewLine, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
|> Seq.map extractDigitCharacters // Extract digit substrings
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
