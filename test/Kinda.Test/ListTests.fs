module Kinda.Test.ListTests

open Expecto
open FsCheck

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

open Kinda.List

type ListGen = 
    static member List () =
        Arb.generate<int list>
        |> Gen.map List.fromList
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<ListGen> ] }

[<Tests>]
let ``List Tests`` =

    testList "List Tests" [
      functorLaws fsCheckConfig defaultEquality ListMonad.Instance
      applicativeLaws defaultEquality ListMonad.Instance
      monadLaws defaultEquality ListMonad.Instance
    ]
