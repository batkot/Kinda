module Kinda.ToDo.Cli.Program

open Kinda.Hangman.Cli.Rules
open Kinda.Hangman.Cli.Tagless
open Kinda.Hangman.Cli.Free
open System
open CommandLine

type Mode
    = FreeMonad = 0
    | Tagless = 1

type Options = 
    { [<Option(Default = Mode.FreeMonad, HelpText = "Program version to run (FreeMonad|Tagless)")>] Mode : Mode
      [<Option(Required=true)>] Puzzle: string
      [<Option(Default=10)>] Chances: int
    }

let run options = 
    let game = newGame options.Puzzle options.Chances
    match options.Mode with
    | Mode.FreeMonad -> runFree game |> ignore
    | Mode.Tagless -> runTagless game |> ignore
    | _ -> failwith ":("

[<EntryPoint>]
let main argv =
    let parseResult = CommandLine.Parser.Default.ParseArguments<Options>(argv)

    match parseResult with
    | :? Parsed<Options> as parsed -> run parsed.Value
    | :? NotParsed<Options> as notParsed -> printfn "Failed"

    0