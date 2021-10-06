module Kinda.Test.FreeTests

open Expecto
open Expecto.Flip

open FsCheck

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests
open Kinda.Test.ReaderTTests
open Kinda.Test.StateTTests

open Kinda.App
open Kinda.Functor
open Kinda.Monad
open Kinda.List
open Kinda.Identity
open Kinda.Free
open Kinda.StateT
open Kinda.ReaderT

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
                let idNt = { new NaturalTransformation<'M,'M> with member _.Transform x = x }
                let freeM = monadicAction free |> FreeH.Project
                let monadResult = monadicAction monad

                runFree monad idNt freeM
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
                { new NaturalTransformation<TestFunctorH, App<StateTH<string list>, IdentityH>> with 
                    member _.Transform (x: App<TestFunctorH, 'a>) =
                        match TestFunctorH.Project x with
                        | Tell (msg, a) ->
                            monad StateMonad.Instance {
                                let! state = State.get
                                do! State.put (state @ [msg])
                                return a
                        }
                }

            let (state, result) = 
                FreeH.Project freeResult
                |> runFree StateMonad.Instance transformToState
                |> State.run []

            let expectedLog = 
                [0; 3; 6] |> List.map (sprintf "Adding 3 to %d")

            Expect.equal "Result should be computed" 9 result
            Expect.equal "State should be computed" expectedLog state
    ]