module Kinda.Test.IdentityTests

open Expecto
open FsCheck

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

open Kinda.Identity

type IdentityGen = 
    static member Identity () =
        Arb.generate<int>
        |> Gen.map Identity.fromA
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<IdentityGen> ] }

[<Tests>]
let ``Identity Tests`` =

    testList "Identity Tests" [
      functorLaws fsCheckConfig defaultEquality IdentityMonad.Instance
      applicativeLaws defaultEquality IdentityMonad.Instance
      monadLaws defaultEquality IdentityMonad.Instance
    ]
