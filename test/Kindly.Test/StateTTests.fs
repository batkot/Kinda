module Kindly.Test.StateTTests

open Expecto
open Expecto.Flip

open Kindly.App
open Kindly.Monad
open Kindly.StateT

[<Tests>]
let tests = 
    testList "State Tests" [
        testList "Should maintain state" [
            let stateAction (f: 's -> 's) = 
                State.get<'s> 
                |> StateTH.Inject
                |> StateMonad.Instance.Map f
                |> Monad.flipBind StateMonad.Instance (State.put >> StateTH.Inject)

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