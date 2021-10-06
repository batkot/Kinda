module Kinda.Hangman.Cli.Tagless

open Kinda.App
open Kinda.Monad
open Kinda.StateT
open Kinda.Identity

open Kinda.Hangman.Cli.Rules

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
    let stack = StateMonad<Game>()
    let monad = stack :> Monad<_>
    
    //Here I'm using the same cheat with Identity
    //To fake IO
    let writeLineIO line = 
        printfn $"{line}"
        Identity.fromA ()

    let getCharIO () =
        System.Console.ReadKey().KeyChar
        |> Identity.fromA

    interface Monad<App<StateTH<Game>, IdentityH>> with
        member _.Map f x = monad.Map f x
        member _.Pure x = monad.Pure x
        member _.Apply fab a = monad.Apply fab a
        member _.Bind ma f = monad.Bind ma f

    interface HangmanMonad<App<StateTH<Game>, IdentityH>> with
        member _.WriteLine (line: string) =
            stack.Lift <| writeLineIO line

        member _.GuessNextLetter () = 
            stack.Lift <| getCharIO ()

        member _.GetGame : App<App<StateTH<Game>, IdentityH>, Game> =
            StateT.get stack.InnerMonad

        member _.SetGame (game: Game) : App<App<StateTH<Game>, IdentityH>, unit> =
            StateT.put stack.InnerMonad game

let runTagless puzzle =
    let monad = HangmanMonadStack()
    taglessHangman monad
    |> State.run puzzle