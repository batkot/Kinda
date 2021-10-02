module Kindly.Test.IdentityTests

open Expecto
open FsCheck

open Kindly.Test.FunctorTests
open Kindly.Test.ApplicativeTests
open Kindly.Test.MonadTests

open Kindly.App
open Kindly.Identity

type IdentityGen = 
    static member Identity () =
        gen {
            let! value = Arb.generate<int>
            return IdentityMonad.Instance.Pure value
        }
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<IdentityGen> ] }

[<Tests>]
let ``Identity Tests`` =

    testList "Identity Tests" [
      functorLaws fsCheckConfig defaultEquality IdentityMonad.Instance
      applicativeLaws defaultEquality IdentityMonad.Instance
      monadLaws defaultEquality IdentityMonad.Instance
    ]
