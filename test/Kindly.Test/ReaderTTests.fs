module Kindly.Test.ReaderTTests

open Expecto
open Expecto.Flip

open FsCheck

open Kindly.Monad
open Kindly.Identity
open Kindly.ReaderT

open Kindly.Test.FunctorTests
open Kindly.Test.ApplicativeTests
open Kindly.Test.MonadTests

let readerEq env = 
    { new Eq<ReaderTH<'r, Identity>> with
        member _.AreEqual x y = 
            let run = ReaderTH.Run env >> Identity.Run
            run x = run y
    }

type ReaderGen = 
    static member Reader () =
        gen {
            let! f = Arb.generate<string -> int>

            return monad ReaderMonad.Instance {
                let! env = Reader.ask |> ReaderTH.Inject
                return f env
            }
        }
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<ReaderGen> ] }

[<Tests>]
let tests = 
    testList "Reader Tests" [
        functorLaws fsCheckConfig (readerEq "reader") ReaderMonad.Instance
        applicativeLaws (readerEq "reader") ReaderMonad.Instance
        monadLaws (readerEq 16) ReaderMonad.Instance

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