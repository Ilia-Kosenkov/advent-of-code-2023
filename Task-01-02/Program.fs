open System

// Match possible digit representation to digit
let getDigitWhenNotEmpty (subString : ReadOnlySpan<char>) =
    let firstDigit = int subString[0]
    if firstDigit >= int '0' && firstDigit <= int '9' then Some(firstDigit - int '0')
    elif subString.StartsWith("one") then Some(1)
    elif subString.StartsWith("two") then Some(2)
    elif subString.StartsWith("three") then Some(3)
    elif subString.StartsWith("four") then Some(4)
    elif subString.StartsWith("five") then Some(5)
    elif subString.StartsWith("six") then Some(6)
    elif subString.StartsWith("seven") then Some(7)
    elif subString.StartsWith("eight") then Some(8)
    elif subString.StartsWith("nine") then Some(9)
    else None
        

// Check string starting at `index` for digit representation
let getDigit (wholeString : string) (index : int) =
    let subString = wholeString.AsSpan(index)
    if subString.IsEmpty then None
    else getDigitWhenNotEmpty subString

// Look for the first occurence of digit representation in a string,
// slicing using offsets from sequence
let findFirstDigit (wholeString : string) (sequence : int seq) =
    sequence
        |> Seq.choose (getDigit wholeString)
        |> Seq.tryHead

// Look for the first number from the start
// Look for the first number from the end
let extractDigitCharacters (line: string) =
    let first = {0..(line.Length - 1)} |> findFirstDigit line
    let last = {(line.Length - 1)..(-1)..0} |> findFirstDigit line
    
    match (first, last) with
    | (Some f, Some l) -> Some(f, l)
    | _ -> None
        
 // The rest is the same
let constructNumber pairOpt =
    pairOpt |> Option.map (fun (f, l) -> f * 10 + l)

// Read input from stdin (new-line-separated string passed as first arg)
Environment
    .GetCommandLineArgs()
    .[1].Split(Environment.NewLine, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
|> Seq.map extractDigitCharacters // Extract digit substrings
|> Seq.map constructNumber // Construct check sums
|> Seq.fold // Compute sum if all check sums are Some, otherwise return None
    (fun sum lineCheckSum ->
        match (sum, lineCheckSum) with
        | (_, None) -> None
        | (None, _) -> None
        | Some s, Some lcs -> Some(s + lcs))
    (Some 0) // Start with 0. Is any of the check sums are None, accumulator is replaced with None
|> printfn "%A"
