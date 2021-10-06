module Kinda.Test.StateTTests

open Expecto
open Expecto.Flip

open FsCheck

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

open Kinda.App
open Kinda.Monad
open Kinda.Identity
open Kinda.StateT

let stateEq state = 
    { new Eq<App<StateTH<'s>, IdentityH>> with
        member _.AreEqual x y =
            let run = State.run state
            run x = run y
    }

type StateGen = 
    static member State () =
        gen {
            let! f = Arb.generate<string -> string * int>

            return monad StateMonad.Instance {
                let! state = State.get
                let (newState, x) = f state
                do! State.put newState
                return x
            }
        }
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<StateGen> ] }

[<Tests>]
let tests = 
    testList "State Tests" [
        functorLaws fsCheckConfig (stateEq "State") StateMonad.Instance
        applicativeLaws (stateEq 10) StateMonad.Instance
        monadLaws (stateEq 10) StateMonad.Instance

        testList "Should maintain state" [
            let stateAction (f: 's -> 's) = 
                monad StateMonad.Instance {
                    let! state = State.get<'s>

                    do! f state |> State.put
                }

            testProperty "Int" <| 
                fun (initState: int) (added: int) -> 
                    stateAction ((+) added)
                    |> State.run initState
                    |> fst
                    |> Expect.equal "State should be maintained" (initState + added)
        ]

        testList "Should return computed value based on state" [
            let stateAction (f: 's -> 'x) = 
                State.get<'s> 
                |> (StateMonad.Instance :> Monad<_>).Map f

            testProperty "Int" <|
                fun (initState: int) (added: int) -> 
                    stateAction ((+) added)
                    |> State.run initState
                    |> snd
                    |> Expect.equal "Should compute result based on state" (initState + added)
        ]
    ]