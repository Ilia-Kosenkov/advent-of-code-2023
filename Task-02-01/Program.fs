open System

Environment
    .GetCommandLineArgs()
    .[1].Split(Environment.NewLine, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)