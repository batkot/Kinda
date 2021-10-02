module Kindly.Test.FreeTests

open Expecto
open Expecto.Flip

open FsCheck

open Kindly.Test.FunctorTests
open Kindly.Test.ApplicativeTests
open Kindly.Test.MonadTests
open Kindly.Test.ReaderTTests
open Kindly.Test.StateTTests

open Kindly.App
open Kindly.Functor
open Kindly.Monad
open Kindly.List
open Kindly.Identity
open Kindly.Free
open Kindly.StateT
open Kindly.ReaderT

let private freeIdentity = FreeMonad(IdentityMonad.Instance)

type FreeGen = 
    static member Free () =
        gen {
            let! x = Arb.generate<int>
            return monad freeIdentity { return x }
        }
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<FreeGen> ] }

type TestFunctorF<'a> = Tell of string * 'a
type TestFunctorH =
    static member Inject (x: TestFunctorF<'a>) : App<TestFunctorH, 'a> = 
        create x
    static member Project (app: App<TestFunctorH, 'a>) : TestFunctorF<'a> =
        unwrap app :?> _

type TestFunctorFunctor () = 
    interface Functor<TestFunctorH> with
        member _.Map f x = 
            x 
            |> TestFunctorH.Project 
            |> (fun (Tell (msg, a)) -> Tell (msg, f a)) 
            |> TestFunctorH.Inject

type TestFunctor<'a> = App<FreeH<TestFunctorH>, 'a>

let tell (msg: string) : TestFunctor<unit> = 
    Tell (msg, Pure ())
    |> TestFunctorH.Inject
    |> Free
    |> FreeH.Inject

let testMonad = FreeMonad(TestFunctorFunctor())

[<Tests>]
let tests = 
    testList "Free Monad Tests" [
        functorLaws fsCheckConfig defaultEquality freeIdentity
        applicativeLaws defaultEquality freeIdentity
        monadLaws defaultEquality freeIdentity

        testList "Applied to monad should act as monad" [
            let monadicAction m =
                monad m {
                    let! x = m.Pure 10
                    return! m.Pure (x + 10)
                }

            let test (eq: Eq<'M>) (monad: Monad<'M>)= 
                let free = FreeMonad(monad)
                let idNaturalTransform = { new NaturalTransform<'M,'M> with member _.Transform x = x }
                let freeM = monadicAction free |> FreeH.Project
                let monadResult = monadicAction monad

                runFree monad idNaturalTransform freeM
                |> eq.AreEqual monadResult
                |> Expect.isTrue "Results from Free and Monad should be the same"

            testCase "Identity" <| fun () -> test defaultEquality IdentityMonad.Instance
            testCase "List" <| fun () -> test defaultEquality ListMonad.Instance
            testCase "State" <| fun () -> test (stateEq 10) StateMonad.Instance
            testCase "Reader" <| fun () -> test (readerEq "Reader") ReaderMonad.Instance
        ]

        testCase "Can interpret FreeMonad into Monad" <| fun () ->
            let add3 x = monad testMonad {
                do! tell $"Adding 3 to {x}"
                return x + 3
            }

            let freeResult = monad testMonad {
                let! x1 = add3 0
                let! x2 = add3 x1
                return! add3 x2
            } 

            let transformToState =
                { new NaturalTransform<TestFunctorH, StateTH<string list, Identity>> with 
                    member _.Transform (x: App<TestFunctorH, 'a>) =
                        match TestFunctorH.Project x with
                        | Tell (msg, a) ->
                            monad StateMonad.Instance {
                                let! state = State.get |> StateTH.Inject
                                do! State.put (state @ [msg]) |> StateTH.Inject
                                return a
                        }
                }

            let (state, result) = 
                FreeH.Project freeResult
                |> runFree StateMonad.Instance transformToState
                |> StateTH.Run []
                |> Identity.Run

            let expectedLog = 
                [ "Adding 3 to 0"
                  "Adding 3 to 3"
                  "Adding 3 to 6"
                ]

            Expect.equal "Result should be computed" 9 result
            Expect.equal "State should be computed" expectedLog state
    ]