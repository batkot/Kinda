module Kinda.Hangman.Cli.Tagless

open Kinda.App
open Kinda.Monad
open Kinda.StateT
open Kinda.IO

open Kinda.Hangman.Cli.Rules
open Kinda.Hangman.Cli.IO

type HangmanMonad<'M> =
    abstract WriteLine: string -> App<'M, unit>
    abstract GuessNextLetter: unit -> App<'M, char>
    abstract GetGame: App<'M, Game>
    abstract SetGame: Game -> App<'M, unit>

let rec taglessHangman<'S,'M when 'S :> Monad<'M> and 'S :> HangmanMonad<'M>>
    (m: 'S) = 
    monad m {
        let! puzzle = m.GetGame
        do! m.WriteLine $"Puzzle is {showPuzzle puzzle}"
        let! letter = m.GuessNextLetter ()
        do! m.WriteLine ""
        match checkLetter letter puzzle with
        | Continue newPuzzle -> 
            do! m.SetGame newPuzzle
            return! taglessHangman m
        | Won ->
            do! m.WriteLine "You won!"
            return ()
        | Lost ->
            do! m.WriteLine "You lost!"
            return ()
    }

type HangmanMonadStack () = 
    let stack = StateTMonad<Game, IOH, _>(IOMonad())
    let monad = stack :> Monad<_>

    interface Monad<App<StateTH<Game>, IOH>> with
        member _.Map f x = monad.Map f x
        member _.Pure x = monad.Pure x
        member _.Apply fab a = monad.Apply fab a
        member _.Bind ma f = monad.Bind ma f

    interface HangmanMonad<App<StateTH<Game>, IOH>> with
        member _.WriteLine (line: string) =
            stack.Lift <| writeLineIO line

        member _.GuessNextLetter () = 
            stack.Lift <| getCharIO ()

        member _.GetGame : App<App<StateTH<Game>, IOH>, Game> =
            StateT.get stack.InnerMonad

        member _.SetGame (game: Game) : App<App<StateTH<Game>, IOH>, unit> =
            StateT.put stack.InnerMonad game

let runTagless puzzle =
    let monad = HangmanMonadStack()
    taglessHangman monad
    |> StateT.run puzzle
    |> IO.run