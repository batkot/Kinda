// Sketchbook
module Kindly.Test.Experimental

open Expecto

open Kindly.App
open Kindly.Monad
open Kindly.Identity
open Kindly.StateT
open Kindly.ReaderT

type StateMonadClass<'s,'M> =
    // inherit Monad<'M>
    abstract member Get : App<'M,'s>
    abstract member Put : 's -> App<'M,unit>

type ReaderMonadClass<'r,'M> =
    // inherit Monad<'M>
    abstract member Ask : App<'M, 'r>

module Generics = 
    let justReader<'M when 'M :> ReaderMonadClass<int, 'M>> (m: 'M) = m.Ask
    let justState<'M when 'M :> StateMonadClass<int, 'M>> (m: 'M) = m.Put

    let both m = monad m {
        let! x = justReader m
        do! justState m x
        return x 
    }

    let genericTest<'r, 's, 'M when 'M :> Monad<'M> and 'M :> StateMonadClass<'s, 'M> and 'M :> ReaderMonadClass<'r,'M>> 
        (m: 'M) =
            monad m {
                let! st = m.Get
                let! env = m.Ask
                do! m.Put st
                return 1
            }

    let genericTest2<'r, 's, 'S, 'M when 'S :> Monad<'M> and 'S :> StateMonadClass<'s, 'M> and 'S :> ReaderMonadClass<'r,'M>> 
        (m: 'S) =
            monad m {
                let! st = m.Get
                let! env = m.Ask
                do! m.Put st
                return 1
            }

    let monadStack<'r,'s> () = ReaderTMonad(StateMonad()) :> Monad<ReaderTH<'r, StateTH<'s, Identity>>>

    type MyStackTH = ReaderTH<string, StateTH<int, Identity>>

    type MyStack () =
        let stateMonad = StateMonad.Instance
        let stack = ReaderTMonad(stateMonad) :> Monad<MyStackTH>

        member _.Run (env: string) (state: int) (x: App<MyStackTH, 'a>) : (int * 'a)=
            x
            |> ReaderTH.Run env 
            |> StateTH.Run state
            |> Identity.Run

        interface Monad<MyStackTH> with
            member _.Map f x = stack.Map f x
            member _.Pure x = stack.Pure x
            member _.Apply fab a = stack.Apply fab a
            member _.Bind ma f = stack.Bind ma f

        interface StateMonadClass<int, MyStackTH> with
            member _.Get = ReaderT (fun _ -> State.get |> StateTH.Inject) |> ReaderTH.Inject
            member _.Put x = ReaderT (fun _ -> State.put x |> StateTH.Inject) |> ReaderTH.Inject

        interface ReaderMonadClass<string, MyStackTH> with
            member _.Ask = ReaderT.ask<_, string> stateMonad |> ReaderTH.Inject

    let myStack = MyStack()

    let addToState<'S, 'M when 'S :> Monad<'M> and 'S :> StateMonadClass<int, 'M>> (m: 'S) x = 
            monad m {
                let! state = m.Get
                let result = state + x
                do! m.Put result
                return result
            } 
    let addFromReader<'S, 'M when 'S :> Monad<'M> and 'S :> ReaderMonadClass<string, 'M>> 
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