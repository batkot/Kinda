// Sketchbook
module Kindly.Test.Experimental

open Expecto

open Kindly.App
open Kindly.Monad
open Kindly.Identity
open Kindly.StateT
open Kindly.ReaderT

type StateMonadClass<'s,'M> =
    inherit Monad<'M>
    abstract member Get : App<'M,'s>
    abstract member Put : 's -> App<'M,unit>

type ReaderMonadClass<'r,'M> =
    inherit Monad<'M>
    abstract member Ask : App<'M, 'r>

module Generics = 
    type MyStackTH = ReaderTH<string, App<StateTH<int>, Identity>>

    type MyStack () =
        let stateMonad = StateMonad.Instance
        let stack = ReaderTMonad(stateMonad)
        let monad = stack :> Monad<MyStackTH>
        // let (trans: MonadTrans<_,_>) = stack :> MonadTrans<MyStackTH, App<StateTH<int>, Identity>>

        member _.Run (env: string) (state: int) (x: App<MyStackTH, 'a>) : (int * 'a)=
            x
            |> ReaderTH.Run env 
            |> State.run state

        interface Monad<MyStackTH> with
            member _.Map f x = monad.Map f x
            member _.Pure x = monad.Pure x
            member _.Apply fab a = monad.Apply fab a
            member _.Bind ma f = monad.Bind ma f

        interface StateMonadClass<int, MyStackTH> with
            member _.Get = failwith "" //trans.Lift (State.get |> StateTH.Inject)
            member _.Put x = failwith "" //trans.Lift (State.put x |> StateTH.Inject)

        interface ReaderMonadClass<string, MyStackTH> with
            member _.Ask = ReaderT.ask<_, string> stateMonad |> ReaderTH.Inject

    let myStack = MyStack()

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