module Kinda.Test.WriterTTests

open Expecto

open FsCheck

open Kinda.Monoid
open Kinda.WriterT

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

let writerMonad = writer Monoid.list

type WriterGen = 
    static member Writer () : Arbitrary<Writer<string list, int>> =
        gen {
            let! x = Arb.generate<int>
            let! writerContent = Arb.generate<string list>
            let w = 
                writerContent
                |> List.map (fun x -> Writer.tell [x])
                |> List.fold (fun a b -> 
                    writerMonad { 
                        do! a
                        return! b
                    }) 
                    (writerMonad { return ()})


            return writerMonad { 
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