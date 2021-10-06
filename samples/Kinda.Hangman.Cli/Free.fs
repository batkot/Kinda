module Kinda.Hangman.Cli.Free

open Kinda.App
open Kinda.Free
open Kinda.Functor
open Kinda.Monad
open Kinda.Void
open Kinda.Identity
open Kinda.IO

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
    let hangmanFunctor = HangmanFunctor() :> Functor<_>
    let liftHangmanF (f: App<HangmanH,'a>)= liftF hangmanFunctor f
    let monad = FreeMonad(hangmanFunctor) :> Monad<_>

    let guessNextLetter () : HangmanFree<char> =
        GuessNextLetter id
        |> HangmanF.inject
        |> liftHangmanF

    let writeLine line : HangmanFree<unit> = 
        WriteLine (line, ())
        |> HangmanF.inject
        |> liftHangmanF

    let getGame () : HangmanFree<Game> =
        GetGame id
        |> HangmanF.inject
        |> liftHangmanF

    let setGame game : HangmanFree<unit> = 
        SetGame (game, ())
        |> HangmanF.inject
        |> liftHangmanF

let rec freeHangmanProgram = 
    monad HangmanFree.monad {
        let! puzzle = HangmanFree.getGame ()
        do! HangmanFree.writeLine $"Puzzle is {showPuzzle puzzle}"
        let! letter = HangmanFree.guessNextLetter ()
        do! HangmanFree.writeLine ""
        match checkLetter letter puzzle with
        | Continue newPuzzle -> 
            do! HangmanFree.setGame newPuzzle
            return! freeHangmanProgram
        | Won ->
            do! HangmanFree.writeLine "You won!"
            return ()
        | Lost ->
            do! HangmanFree.writeLine "You lost!"
            return ()
    }

module Interpreter = 
    let mutable private privGame: Game = newGame "X" 0

    let interpret hangman =
        match HangmanF.project hangman with
        | WriteLine (msg, a) -> 
            io {
                printfn $"{msg}"
                return a
            }
        | GuessNextLetter next ->
            io {
                let char = System.Console.ReadKey().KeyChar
                return next char 
            }
        | GetGame next ->
            io { return next privGame }
        | SetGame (game, a) -> 
            io {
                privGame <- game
                return a
            }

    let nt game = 
        privGame <- game
        { new NaturalTransformation<HangmanH, IOH> with
            member _.Transform x = interpret x }

let runFree puzzle = 
    freeHangmanProgram
    |> runFree IOMonad.Instance (Interpreter.nt puzzle)
    |> IO.run
