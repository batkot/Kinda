module Kinda.Test.StateTTests

open Expecto
open Expecto.Flip
open FsCheck
open Kinda.Test.Helpers

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

open Kinda.Monad
open Kinda.Identity
open Kinda.StateT

let stateEq state = 
    { new Eq<StateTH<'s, IdentityH>> with
        member _.AreEqual x y =
            let run = State.run state
            run x = run y
    }

type TestState = string

type StateGen<'s> = 
    static member Generator : Arbitrary<HktGen<StateH<'s>>> = 
        { new HktGen<StateH<'s>> with 
            member _.Generate<'a> () = 
                Arb.generate<'s-> 's * 'a>
                |> Gen.map State.fromFunction
        } |> HktGen.toArb

let fsCheckConfig<'state> = FsCheckConfig.withFunctorGen<StateGen<'state>>

[<Tests>]
let tests = 
    testList "State Tests" [
        functorLaws fsCheckConfig<string> (stateEq "State") StateMonad.Instance
        applicativeLaws fsCheckConfig<int> (stateEq 20) StateMonad.Instance
        monadLaws (stateEq "State") StateMonad.Instance

        testList "Should maintain state" [
            testProperty "Int" <| 
                fun (initState: int) (added: int) -> 
                    State.fromFunction (fun s -> s + added, ())
                    |> State.run initState
                    |> fst
                    |> Expect.equal "State should be maintained" (initState + added)
        ]

        testList "Should return computed value based on state" [
            testProperty "Int" <|
                fun (initState: int) (added: int) -> 
                    State.fromFunction (fun s -> s, s + added)
                    |> State.run initState
                    |> snd
                    |> Expect.equal "Should compute result based on state" (initState + added)
        ]
    ]