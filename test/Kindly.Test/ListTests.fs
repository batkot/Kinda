module Kindly.Test.ListTests

open Expecto
open FsCheck

open Kindly.Test.FunctorTests
open Kindly.Test.ApplicativeTests
open Kindly.Test.MonadTests

open Kindly.List

type ListGen = 
    static member List () =
        gen {
            let! value = Arb.generate<int>
            return ListMonad.Instance.Pure value
        }
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<ListGen> ] }

[<Tests>]
let ``List Tests`` =

    testList "List Tests" [
      functorLaws fsCheckConfig ListMonad.Instance
      applicativeLaws ListMonad.Instance
      monadLaws ListMonad.Instance
    ]
