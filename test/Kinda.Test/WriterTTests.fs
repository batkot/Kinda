module Kinda.Test.WriterTTests

open Expecto
open FsCheck
open Kinda.Test.Helpers

open Kinda.Monoid
open Kinda.WriterT

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

type TestWriter = string
type TestWriterMonoid = TestWriter list

let writerMonad = writer Monoid.list<string>

type WriterGen = 
    static member Writer () : Arbitrary<HktGen<WriterH<TestWriterMonoid>>> =
        { new HktGen<WriterH<TestWriterMonoid>> with 
            member _.Generate<'a> () =
                gen {
                    let! x = Arb.generate<'a>
                    let! writerContent = Arb.generate<TestWriterMonoid>
                    return Writer.fromTuple (x, writerContent)
                }
        } |> HktGen.toArb

let fsCheckConfig = FsCheckConfig.withFunctorGen<WriterGen>

[<Tests>]
let tests = 
    testList "Writer Monad Tests" [
        functorLaws fsCheckConfig defaultEquality writerMonad
        applicativeLaws fsCheckConfig defaultEquality writerMonad
        monadLaws defaultEquality writerMonad
    ]