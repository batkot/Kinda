module Kinda.Test.ListTests

open Expecto
open FsCheck
open Kinda.Test.Helpers

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

open Kinda.List

type ListGen = 
    static member Generator : Arbitrary<HktGen<ListH>> = 
        { new HktGen<ListH> with 
            member _.Generate<'a> () = 
                Arb.generate<'a list>
                //Perf problems with List
                |> Gen.map (List.truncate 10 >> List.fromList)
        } |> HktGen.toArb

let fsCheckConfig = FsCheckConfig.withFunctorGen<ListGen>

[<Tests>]
let ``List Tests`` =
    testList "List Tests" [
      functorLaws fsCheckConfig defaultEquality ListMonad.Instance
      applicativeLaws fsCheckConfig defaultEquality ListMonad.Instance
      monadLaws fsCheckConfig defaultEquality ListMonad.Instance
    ]
