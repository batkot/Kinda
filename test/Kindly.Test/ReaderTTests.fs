module Kindly.Test.ReaderTTests

open Expecto
open Expecto.Flip

open FsCheck

open Kindly.App
open Kindly.Monad
open Kindly.Identity
open Kindly.ReaderT

open Kindly.Test.FunctorTests
open Kindly.Test.ApplicativeTests
open Kindly.Test.MonadTests

let readerEq env = 
    { new Eq<App<ReaderTH<'r>, Identity>> with
        member _.AreEqual x y = 
            let run = ReaderT.run env
            run x = run y
    }

type ReaderGen = 
    static member Reader () =
        gen {
            let! f = Arb.generate<string -> int>

            return monad ReaderMonad.Instance {
                let! env = Reader.ask
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
                    let! env = Reader.ask

                    return f env
                }

            testProperty "Int" <| 
                fun (env: int) (added: int) -> 
                    readerAction ((+) added)
                    |> Reader.run env
                    |> Expect.equal "Environment should be passed" (env + added)
        ]
    ]