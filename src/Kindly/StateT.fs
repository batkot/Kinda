module Kindly.StateT

open Kindly.App
open Kindly.Identity
open Kindly.Monad

type StateT<'s, 'M, 'a> = StateT of ('s -> App<'M, 's * 'a>)

let runStateT state (StateT st) = st state

type State<'s,'a> = StateT<'s, Identity, 'a>

type StateTH<'s, 'M> = 
    static member Inject (state: StateT<'s, 'M, 'a>) : App<StateTH<'s, 'M>, 'a> =
        create state
    static member Project (app: App<StateTH<'s, 'M>, 'a>) : StateT<'s, 'M, 'a> = 
        unwrap app :?> _

type StateTMonad<'s, 'M> (monad: Monad<'M> ) = 
    interface Monad<StateTH<'s, 'M>> with
        member _.Map (f : 'a -> 'b) (x: App<StateTH<'s, 'M>, 'a>) : App<StateTH<'s, 'M>,'b> =
            StateT <| fun state ->
                StateTH.Project x
                |> runStateT state
                |> monad.Map (fun (st, a) -> st, f a)
            |> StateTH.Inject

        member _.Pure (x : 'a) : App<StateTH<'s,'M>, 'a> = 
            failwith ""

        member _.Apply (fab: App<StateTH<'s,'M>, 'a -> 'b>) (a: App<StateTH<'s,'M>,'a>) : App<StateTH<'s,'M>, 'b> = 
            failwith ""

        member _.Bind (ma: App<StateTH<'s,'M>, 'a>) (f : 'a -> App<StateTH<'s,'M>, 'b>) =
            failwith ""

    static member Instance monad = StateTMonad(monad) :> Monad<StateTH<'s,'M>>
