module Kinda.Test.IdentityTests

open Expecto
open FsCheck

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

open Kinda.App
open Kinda.Identity
open Kinda.Monad

type IdentityGen = 
    static member Identity () =
        gen {
            let! value = Arb.generate<int>
            return (IdentityMonad.Instance :> Monad<Identity>).Pure value
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
