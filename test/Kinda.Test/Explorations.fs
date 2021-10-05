// Sketchbook
module Kinda.Test.Experimental

open Expecto

open Kinda.App
open Kinda.Monad
open Kinda.Identity
open Kinda.StateT
open Kinda.ReaderT

type StateMonadClass<'s,'M> =
    inherit Monad<'M>
    abstract member Get : App<'M,'s>
    abstract member Put : 's -> App<'M,unit>

type ReaderMonadClass<'r,'M> =
    inherit Monad<'M>
    abstract member Ask : App<'M, 'r>

module Generics = 
    type MyStackTH = App<ReaderTH<string>,App<StateTH<int>, Identity>>

    type MyStack () =
        let stack = ReaderTMonad(StateMonad())
        let monad = stack :> Monad<MyStackTH>

        member _.Run (env: string) (state: int) (x: ReaderT<string, App<StateTH<int>, Identity>, 'a>) : (int * 'a)=
            x
            |> ReaderT.run env 
            |> State.run state

        interface Monad<MyStackTH> with
            member _.Map f x = monad.Map f x
            member _.Pure x = monad.Pure x
            member _.Apply fab a = monad.Apply fab a
            member _.Bind ma f = monad.Bind ma f

        interface StateMonadClass<int, MyStackTH> with
            member _.Get = stack.Lift <| StateT.get stack.InnerMonad.InnerMonad
            member _.Put x = stack.Lift <| StateT.put stack.InnerMonad.InnerMonad x

        interface ReaderMonadClass<string, MyStackTH> with
            member _.Ask = ReaderT.ask<_, string> stack.InnerMonad

        member _.Hmm x : App<MyStackTH, 'a> = 
            Id x 
            |> Identity.Inject
            |> stack.InnerMonad.Lift
            |> stack.Lift 


    let myStack = MyStack()

    let bar (m: StateTMonad<int,_, ReaderTMonad<string, _, _>>) : StateT<int, _, int>=
        monad m {
            let! counter = StateT.get m.InnerMonad
            let! text = m.Lift <| ReaderT.ask m.InnerMonad.InnerMonad
            return counter + String.length text
        }

    let addToState<'S, 'M when 'S :> StateMonadClass<int, 'M>> (m: 'S) x = 
            monad m {
                let! state = m.Get
                let result = state + x
                do! m.Put result
                return result
            } 

    let addFromReader<'S, 'M when 'S :> ReaderMonadClass<string, 'M>> 
        (m: 'S) (x: int) = 
            monad m {
                let! env = m.Ask
                return $"{env}-{x}"
            }

    let combined m x = 
        addToState m x 
        |> Monad.flipBind m (addFromReader m)


    [<Tests>]
    let tests = 
        testList "Generic param based type classess"
            [ testCase "X" <| fun () -> 
                let (state, returned) = 
                    monad myStack {
                        let! result = addToState myStack 10
                        return! addFromReader myStack result
                    }
                    |> myStack.Run "Test" 10

                Expect.equal state 20 "State should be maintained"
                Expect.equal returned "Test-20" "Reader should be pushed"
            ]

module Parameters =
    type Dependency<'a, 'b> = 'a -> 'b

    let justReader (reader: ReaderMonadClass<int, 'M>) = reader.Ask

    let parameterTest
        (m: Monad<'M>) 
        (state: StateMonadClass<int,'M>) = 
        monad m {
            let! st = state.Get
            do! state.Put <| st + 15
            return 0
        }

    let uncurriedTest
        ((m, state, reader): Monad<'M> * StateMonadClass<int, 'M> * ReaderMonadClass<int, 'M>) =
        monad m {
            let! env = justReader reader
            let! st = state.Get
            do! state.Put <| st + 15
            return 0
        }