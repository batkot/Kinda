module Kindly.Test.WriterTTests

open Expecto
open Expecto.Flip

open FsCheck

open Kindly.Monad
open Kindly.Identity
open Kindly.WriterT

open Kindly.Test.FunctorTests
open Kindly.Test.ApplicativeTests
open Kindly.Test.MonadTests

let listMonoid = ListMonoid<string> ()
let writerMonad = WriterMonad.MonadInstance listMonoid

type WriterGen = 
    static member Writer () =
        gen {
            let! x = Arb.generate<int>
            let! writerContent = Arb.generate<string list>
            let w = 
                writerContent
                |> List.map (fun x -> Writer.tell [x] |> WriterTH.Inject)
                |> List.fold (fun a b -> 
                    monad writerMonad { 
                        do! a
                        return! b
                    }) 
                    (monad writerMonad { return ()})


            return monad writerMonad { 
                do! w
                return x 
            }
        }
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<WriterGen> ] }

[<Tests>]
let tests = 
    testList "Writer Monad Tests" [
        functorLaws fsCheckConfig defaultEquality writerMonad
        applicativeLaws defaultEquality writerMonad
        monadLaws defaultEquality writerMonad
    ]