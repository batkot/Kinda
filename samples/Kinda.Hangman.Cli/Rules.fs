module Kinda.Hangman.Cli.Rules

type PuzzleLetter =
    | Hidden of char
    | Guessed of char

let private isHidden = function
    | Hidden x -> true
    | _ -> false

type Game =
    private { Puzzle : PuzzleLetter list
              Chances : int
            }

type CheckResult 
    = Continue of Game
    | Won
    | Lost

let newGame (puzzle: string) (chances: int) = 
    { Puzzle = puzzle.ToUpper() |> Seq.map Hidden |> List.ofSeq
      Chances = chances
    }

let showPuzzle ({ Puzzle = puzzle }) = 
    let showLetter = function
        | Hidden _ -> "_"
        | Guessed x -> $"{x}"

    List.map showLetter puzzle
    |> String.concat " "

let checkLetter (letter: char) (game: Game): CheckResult =
    let mutable matchedLetter = 1
    let flipLetter x = function
        | Hidden y when x = y -> 
            matchedLetter <- 0
            Guessed x
        | y -> y

    let x = System.Char.ToUpper letter
    let newGame = 
        { game with 
            Puzzle = List.map (flipLetter x) game.Puzzle 
            Chances = game.Chances - matchedLetter
        } 

    let solved = List.forall (isHidden >> not) newGame.Puzzle 
    let outOfChances = newGame.Chances < 1
    match solved, outOfChances with
    | true, _ -> Won
    | _, true -> Lost
    | _, _ -> Continue newGame