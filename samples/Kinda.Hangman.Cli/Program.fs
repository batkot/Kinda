module Kinda.ToDo.Cli.Program

open System
open CommandLine

type Mode
    = FreeMonad = 0
    | Tagless = 1

type Options = 
    { [<Option(Default = Mode.FreeMonad, HelpText = "Program version to run (FreeMonad|Tagless)")>] Mode : Mode
      [<Option(Required=true)>] Puzzle: string
    }

let run options = 
    match options.Mode with
    | Mode.FreeMonad -> "Free Monad version"
    | Mode.Tagless -> "Tagless Final version"
    | _ -> "Freakin' parsing"

[<EntryPoint>]
let main argv =
    let parseResult = CommandLine.Parser.Default.ParseArguments<Options>(argv)

    match parseResult with
    | :? Parsed<Options> as parsed -> printfn "%s" <| run parsed.Value
    | :? NotParsed<Options> as notParsed -> printfn "Failed"

    0