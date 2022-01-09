module Kinda.Test.ReaderTTests

open Expecto
open Expecto.Flip
open FsCheck
open Kinda.Test.Helpers

open Kinda.Monad
open Kinda.Identity
open Kinda.ReaderT

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

let readerEq env = 
    { new Eq<ReaderTH<'r, IdentityH>> with
        member _.AreEqual x y = 
            let run = ReaderT.run env
            run x = run y
    }

type TestEnv = string

type ReaderGen<'r> = 
    static member Generator : Arbitrary<HktGen<ReaderH<'r>>> =
        { new HktGen<ReaderH<'r>> with 
            member _.Generate<'a> () = 
                Arb.generate<'r -> 'a>
                |> Gen.map Reader.fromFunction
        } |> HktGen.toArb

let fsCheckConfig = FsCheckConfig.withFunctorGen<ReaderGen<TestEnv>>

[<Tests>]
let tests = 
    testList "Reader Tests" [
        functorLaws fsCheckConfig (readerEq "reader") ReaderMonad.Instance
        applicativeLaws fsCheckConfig (readerEq "reader") ReaderMonad.Instance
        monadLaws fsCheckConfig (readerEq "reader") ReaderMonad.Instance

        testList "Should return computed value based on environment" [
            testProperty "Int" <| 
                fun (env: int) (added: int) -> 
                    Reader.fromFunction ((+) added)
                    |> Reader.run env
                    |> Expect.equal "Environment should be passed" (env + added)
        ]
    ]