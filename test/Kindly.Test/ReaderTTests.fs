module Kindly.Test.ReaderTTests

open Expecto
open Expecto.Flip

open Kindly.Monad
open Kindly.ReaderT

[<Tests>]
let tests = 
    testList "Reader Tests" [
        testList "Should return computed value based on environment" [
            let readerAction (f: 'r -> 'a) = 
                monad ReaderMonad.Instance {
                    let! env = Reader.ask |> ReaderTH.Inject

                    return f env
                }

            testProperty "Int" <| 
                fun (initState: int) (added: int) -> 
                    readerAction ((+) added)
                    |> ReaderTH.Project
                    |> runReader initState
                    |> Expect.equal "Environment should be passed" (initState + added)
        ]
    ]