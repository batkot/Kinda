module Kinda.Test.IdentityTests

open Expecto
open FsCheck
open Kinda.Test.Helpers

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

open Kinda.Identity

type IdentityGen = 
    static member Generator : Arbitrary<HktGen<IdentityH>> =
        { new HktGen<IdentityH> with 
            member _.Generate<'a> () = 
                Arb.generate<'a>
                |> Gen.map Identity.fromA
        } |> HktGen.toArb

let fsCheckConfig = FsCheckConfig.withFunctorGen<IdentityGen>

[<Tests>]
let ``Identity Tests`` =
    testList "Identity Tests" [
      functorLaws fsCheckConfig defaultEquality IdentityMonad.Instance
      applicativeLaws fsCheckConfig defaultEquality IdentityMonad.Instance
      monadLaws defaultEquality IdentityMonad.Instance
    ]
