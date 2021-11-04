module Kinda.ToDo.Cli.Program

open Kinda.Hangman.Cli.Rules
open Kinda.Hangman.Cli.Tagless
open Kinda.Hangman.Cli.Free
open Kinda.Hangman.Cli.Transformers
open CommandLine
open CommandLine.Text

type Mode
    = FreeMonad = 0
    | Tagless = 1
    | Transformers = 2

type Options = 
    { [<Option(Default = Mode.FreeMonad, HelpText = "Program version to run")>] Mode : Mode
      [<Option(Required=true)>] Puzzle: string
      [<Option(Default=10)>] Chances: int
    }

let run options = 
    let game = newGame options.Puzzle options.Chances
    match options.Mode with
    | Mode.FreeMonad -> runFree game |> ignore
    | Mode.Tagless -> runTagless game |> ignore
    | Mode.Transformers -> runTransformers game |> ignore
    | _ -> failwith ":("

let displayHelp<'a> (error: NotParsed<'a>) = 
    let x =
        HelpText.AutoBuild
            ( error
            , fun opt -> 
                opt.AddEnumValuesToHelpText <- true
                HelpText.DefaultParsingErrorsHandler(error, opt)
            , id)

    x.ToString()

[<EntryPoint>]
let main argv =
    let parser = new Parser(fun x -> x.HelpWriter <- null)
    let parseResult = parser.ParseArguments<Options>(argv)

    match parseResult with
    | :? Parsed<Options> as parsed -> run parsed.Value
    | :? NotParsed<Options> as notParsed -> printfn "%A" <| displayHelp notParsed

    0