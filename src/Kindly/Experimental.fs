// Sketchbook
module private Kindly.Experimental

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

    // Constraints infered
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

    type MyStack (stack: Monad<MyStackTH>) =
        member _.Run env state =
            ReaderTH.Run env 
            >> StateTH.Run state
            >> Identity.Project
            >> Identity.runIdentity

        interface Monad<MyStackTH> with
            member _.Map f x = stack.Map f x
            member _.Pure x = stack.Pure x
            member _.Apply fab a = stack.Apply fab a
            member _.Bind ma f = stack.Bind ma f

        interface StateMonadClass<int, MyStackTH> with
            member _.Get = failwith ""
            member _.Put _ = failwith ""

        interface ReaderMonadClass<string, MyStackTH> with
            member _.Ask = failwith ""

    let stack = MyStack(monadStack<string, int> ())
    let wut = genericTest2 stack 

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