module Kinda.Hangman.Cli.IO

open Kinda.IO

let writeLineIO line = 
    io { return printfn $"{line}" }

let getCharIO () =
    io { return System.Console.ReadKey().KeyChar }

