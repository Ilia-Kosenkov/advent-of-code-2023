open System

let splitOptions =
    StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries

Environment.GetCommandLineArgs().[1].Split(Environment.NewLine, splitOptions)
|> Array.iter (printfn "%A")
