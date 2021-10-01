module Kindly.StateT

open Kindly.App
open Kindly.Identity
open Kindly.Monad

type StateT<'s, 'M, 'a> = StateT of ('s -> App<'M, 's * 'a>)

module StateT =
    let get (monad: Monad<'M>) : StateT<'s, 'M, 's> = 
        StateT <| fun state -> monad.Pure (state, state)

    let put (monad: Monad<'M>) (state: 's): StateT<'s, 'M, unit> = 
        StateT <| fun _ -> monad.Pure (state, ())

let runStateT state (StateT st) = st state

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
            StateT <| fun state -> monad.Pure (state, x) 
            |> StateTH.Inject

        member _.Apply (fab: App<StateTH<'s,'M>, 'a -> 'b>) (a: App<StateTH<'s,'M>,'a>) : App<StateTH<'s,'M>, 'b> = 
            StateT <| fun state -> 
                let fab' = 
                    StateTH.Project fab
                    |> runStateT state

                monad.Bind fab' 
                    (fun (state1, f) -> StateTH.Project a |> runStateT state1 |> monad.Map (fun (state2, a) -> (state2, f a)))
            |> StateTH.Inject

        member _.Bind (ma: App<StateTH<'s,'M>, 'a>) (f : 'a -> App<StateTH<'s,'M>, 'b>) =
            StateT <| fun state ->
                let ma' = 
                    StateTH.Project ma
                    |> runStateT state

                monad.Bind ma'
                    (fun (state1, a) -> f a |> StateTH.Project |> runStateT state1)
            |> StateTH.Inject


    static member Instance<'s> monad = StateTMonad(monad) :> Monad<StateTH<'s,'M>>

type State<'s,'a> = StateT<'s, Identity, 'a>

let runState state = runStateT state >> Identity.Project >> runIdentity

type StateMonad<'s> () = 
    inherit StateTMonad<'s, Identity>(IdentityMonad.Instance)
    
    static member Instance = StateMonad<'s>() :> Monad<StateTH<'s,Identity>>

module State =
    let get<'s> = StateT.get<Identity, 's> IdentityMonad.Instance 

    let put (state: 'state) = StateT.put IdentityMonad.Instance state