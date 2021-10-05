module Kinda.Hangman.Cli.Free

open Kinda.App
open Kinda.Free
open Kinda.Functor
open Kinda.Monad
open Kinda.Void
open Kinda.Identity

open Kinda.Hangman.Cli.Rules

type HangmanF<'a>
    = WriteLine of string * 'a
    | GuessNextLetter of (char -> 'a)
    | GetGame of (Game -> 'a)
    | SetGame of Game * 'a

type HangmanH = Void

module private HangmanF =
    let inject (hangman: HangmanF<'a>): App<HangmanH, 'a> =
        create hangman

    let project (app: App<HangmanH, 'a>): HangmanF<'a> =
        unwrap app :?> _

    let map (f: 'a -> 'b) (x: App<HangmanH, 'a>) : App<HangmanH, 'b>=
        match project x with
        | WriteLine (msg, a) -> WriteLine (msg, f a)
        | GuessNextLetter next -> GuessNextLetter (next >> f)
        | GetGame next -> GetGame (next >> f)
        | SetGame (game, a) -> SetGame (game, f a)
        |> inject

type HangmanFunctor () = 
    interface Functor<HangmanH> with
        member _.Map f x = HangmanF.map f x

type HangmanFree<'a> = App<FreeH<HangmanH>, 'a>

module HangmanFree = 
    let monad = FreeMonad(HangmanFunctor()) :> Monad<_>
    let guessNextLetter () : HangmanFree<char> =
        GuessNextLetter (monad.Pure >> FreeH.Project)
        |> HangmanF.inject
        |> Free
        |> FreeH.Inject

    let writeLine line : HangmanFree<unit> = 
        WriteLine (line, Pure ())
        |> HangmanF.inject
        |> Free
        |> FreeH.Inject

    let getGame () : HangmanFree<Game> =
        GetGame (monad.Pure >> FreeH.Project)
        |> HangmanF.inject
        |> Free
        |> FreeH.Inject

    let setGame game : HangmanFree<unit> = 
        SetGame (game, Pure ())
        |> HangmanF.inject
        |> Free
        |> FreeH.Inject


let rec freeHangmanProgram = 
    monad HangmanFree.monad {
        let! puzzle = HangmanFree.getGame ()
        do! HangmanFree.writeLine $"Puzzle is {showPuzzle puzzle}"
        let! letter = HangmanFree.guessNextLetter ()
        do! HangmanFree.writeLine ""
        match checkLetter letter puzzle with
        | Ok newPuzzle -> 
            do! HangmanFree.setGame newPuzzle
            return! freeHangmanProgram
        | Error (Won _) ->
            do! HangmanFree.writeLine "You won!"
            return ()
        | Error (Lost _) ->
            do! HangmanFree.writeLine "You lost!"
            return ()
    }

module Interpreter = 
    //Cheat, because we're not running it in Identity
    //But F# is not pure
    let mutable private privGame: Game = newGame "X" 0

    let interpret hangman =
        match HangmanF.project hangman with
        | WriteLine (msg, a) -> 
            printfn $"{msg}"
            Id a |> Identity.Inject
        | GuessNextLetter next ->
            let char = System.Console.ReadKey().KeyChar
            next char
            |> Id |> Identity.Inject
        | GetGame next ->
            next privGame |> Id |> Identity.Inject
        | SetGame (game, a) -> 
            privGame <- game
            Id a |> Identity.Inject

    let nt game = 
        privGame <- game
        { new NaturalTransformation<HangmanH, Identity> with
            member _.Transform x = interpret x }

let runFree puzzle = 
    freeHangmanProgram
    |> FreeH.Project
    |> runFree IdentityMonad.Instance (Interpreter.nt puzzle)
