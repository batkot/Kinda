module Kindly.Test.FreeTests

open Expecto
open Expecto.Flip

open Kindly.Test.FunctorTests
open Kindly.Test.ApplicativeTests
open Kindly.Test.MonadTests

open Kindly.Monad
open Kindly.List
open Kindly.Identity
open Kindly.Free
open Kindly.StateT

[<Tests>]
let tests = 
    testList "Free Monad Tests" [
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
        ]
    ]