module Kindly.Test.StateTTests

open Expecto
open Expecto.Flip

open Kindly.Test.FunctorTests
open Kindly.Test.ApplicativeTests
open Kindly.Test.MonadTests

open Kindly.Monad
open Kindly.Identity
open Kindly.StateT

let stateEq state = 
    { new Eq<StateTH<'s, Identity>> with
        member _.AreEqual x y =
            let run = StateTH.Run state >> Identity.Run
            run x = run y
    }

[<Tests>]
let tests = 
    testList "State Tests" [
        applicativeLaws (stateEq 10) StateMonad.Instance
        monadLaws (stateEq 10) StateMonad.Instance

        testList "Should maintain state" [
            let stateAction (f: 's -> 's) = 
                monad StateMonad.Instance {
                    let! state = State.get<'s> |> StateTH.Inject

                    do! f state
                        |> State.put
                        |> StateTH.Inject
                }

            testProperty "Int" <| 
                fun (initState: int) (added: int) -> 
                    stateAction ((+) added)
                    |> StateTH.Project
                    |> runState initState
                    |> fst
                    |> Expect.equal "State should be maintained" (initState + added)
        ]

        testList "Should return computed value based on state" [
            let stateAction (f: 's -> 'x) = 
                State.get<'s> 
                |> StateTH.Inject
                |> StateMonad.Instance.Map f

            testProperty "Int" <|
                fun (initState: int) (added: int) -> 
                    stateAction ((+) added)
                    |> StateTH.Project
                    |> runState initState
                    |> snd
                    |> Expect.equal "Should compute result based on state" (initState + added)
        ]
    ]