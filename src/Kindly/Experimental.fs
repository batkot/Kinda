// Sketchbook
module private Kindly.Experimental

open Kindly.App
open Kindly.Monad
open Kindly.StateT

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

    let x = 
        { new ReaderMonadClass<int, int> with 
            member _.Ask = failwith ""

          interface StateMonadClass<int, int> with
            member _.Get = failwith ""
            member _.Put _ = failwith ""

          interface Monad<int> with 
            member _.Map _ _ = failwith ""
            member _.Pure _ = failwith ""
            member _.Apply _ _ = failwith ""
            member _.Bind _ _ = failwith ""
        }
    
    // Crap :/
    // let wut = both x

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