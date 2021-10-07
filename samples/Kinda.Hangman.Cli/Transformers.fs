module Kinda.Hangman.Cli.Transformers

open Kinda.StateT
open Kinda.IO

open Kinda.Hangman.Cli.IO
open Kinda.Hangman.Cli.Rules

let monadStack = stateT io

let rec transformerHangman =
    monadStack {
        let! puzzle = StateT.get monadStack.Monad.InnerMonad
        do! monadStack.Lift <| writeLineIO $"Puzzle is {showPuzzle puzzle}"
        let! letter = monadStack.Lift <| getCharIO ()
        do! monadStack.Lift <| writeLineIO ""

        match checkLetter letter puzzle with
        | Continue newPuzzle -> 
            do! StateT.put monadStack.Monad.InnerMonad newPuzzle
            return! transformerHangman
        | Won ->
            do! monadStack.Lift <| writeLineIO $"You won!"
            return ()
        | Lost ->
            do! monadStack.Lift <| writeLineIO $"You lost!"
            return ()
    }

let runTransformers puzzle = 
    transformerHangman
    |> StateT.run puzzle
    |> IO.run