module Kinda.Hangman.Cli.Transformers

open Kinda.StateT
open Kinda.IO
open Kinda.Monad

open Kinda.Hangman.Cli.IO
open Kinda.Hangman.Cli.Rules

let monadStack = stateT io

#nowarn "40"
let rec transformerHangman =
    monadStack {
        let! puzzle = StateT.get <| MonadTrans.innerMonad monadStack
        do! MonadTrans.lift monadStack <| writeLineIO $"Puzzle is {showPuzzle puzzle}"
        let! letter = MonadTrans.lift monadStack <| getCharIO ()
        do! MonadTrans.lift monadStack <| writeLineIO ""

        match checkLetter letter puzzle with
        | Continue newPuzzle -> 
            do! StateT.put (MonadTrans.innerMonad monadStack) newPuzzle
            return! transformerHangman
        | Won ->
            do! MonadTrans.lift monadStack <| writeLineIO $"You won!"
            return ()
        | Lost ->
            do! MonadTrans.lift monadStack <| writeLineIO $"You lost!"
            return ()
    }

let runTransformers puzzle = 
    transformerHangman
    |> StateT.run puzzle
    |> IO.run