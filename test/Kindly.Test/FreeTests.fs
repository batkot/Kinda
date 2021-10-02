module Kindly.Test.FreeTests

open Expecto
open Expecto.Flip

open FsCheck

open Kindly.Test.FunctorTests
open Kindly.Test.ApplicativeTests
open Kindly.Test.MonadTests
open Kindly.Test.ReaderTTests
open Kindly.Test.StateTTests

open Kindly.Monad
open Kindly.List
open Kindly.Identity
open Kindly.Free
open Kindly.StateT
open Kindly.ReaderT

let private freeIdentity = FreeMonad(IdentityMonad.Instance)

type FreeGen = 
    static member Free () =
        gen {
            let! x = Arb.generate<int>
            return monad freeIdentity { return x }
        }
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<FreeGen> ] }

[<Tests>]
let tests = 
    testList "Free Monad Tests" [
        functorLaws fsCheckConfig defaultEquality freeIdentity
        applicativeLaws defaultEquality freeIdentity
        monadLaws defaultEquality freeIdentity

        testList "Applied to monad should act as monad" [
            let monadicAction m =
                monad m {
                    let! x = m.Pure 10
                    return! m.Pure (x + 10)
                }

            let test (eq: Eq<'M>) (monad: Monad<'M>)= 
                let free = FreeMonad(monad)
                let idNaturalTransform = { new NaturalTransform<'M,'M> with member _.Transform x = x }
                let freeM = monadicAction free |> FreeH.Project
                let monadResult = monadicAction monad

                runFree monad idNaturalTransform freeM
                |> eq.AreEqual monadResult
                |> Expect.isTrue "Results from Free and Monad should be the same"

            testCase "Identity" <| fun () -> test defaultEquality IdentityMonad.Instance
            testCase "List" <| fun () -> test defaultEquality ListMonad.Instance
            testCase "State" <| fun () -> test (stateEq 10) StateMonad.Instance
            testCase "Reader" <| fun () -> test (readerEq "Reader") ReaderMonad.Instance
        ]
    ]