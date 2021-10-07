module Kinda.StateT

open Kinda.App
open Kinda.Identity
open Kinda.Monad
open Kinda.Void

type private InnerStateT<'s, 'M, 'a> = MkStateT of ('s -> App<'M, 's * 'a>)

let private runStateT state (MkStateT st) : App<'M, 's * 'a> = st state

let private get (monad: Monad<'M>) : InnerStateT<'s, 'M, 's> = 
    MkStateT <| fun state -> monad.Pure (state, state)

let private put (monad: Monad<'M>) (state: 's): InnerStateT<'s, 'M, unit> = 
    MkStateT <| fun _ -> monad.Pure (state, ())

type StateTH<'s> = private STH of Void

type StateT<'s,'M,'a> = App<App<StateTH<'s>, 'M>, 'a>

module StateT =
    let private inject (state: InnerStateT<'s,'M,'a>) : StateT<'s,'M,'a> = 
        create state
    
    let private project (app: StateT<'s,'M,'a>) : InnerStateT<'s,'M,'a> =
        unwrap app :?> _

    let get (monad: Monad<'M>): App<App<StateTH<'s>, 'M>, 's> =
        get monad |> inject

    let put (monad: Monad<'M>) (state: 's): App<App<StateTH<'s>, 'M>, unit> =
        put monad state |> inject

    let run (state: 's) (x: App<App<StateTH<'s>, 'M>, 'a>) = project x |> runStateT state

    let fromFunction (stateT: 's -> App<'M, 's * 'a>) : StateT<'s,'M,'a>=
        MkStateT stateT |> inject

type StateTMonad<'s, 'M, 'MI when 'MI :> Monad<'M>> (innerMonad: 'MI) = 

    interface Monad<App<StateTH<'s>, 'M>> with
        member _.Map (f : 'a -> 'b) (x: StateT<'s,'M,'a>) : StateT<'s,'M, 'b> =
            StateT.fromFunction <| fun state ->
                StateT.run state x
                |> innerMonad.Map (fun (st, a) -> st, f a)

        member _.Pure (x : 'a) : StateT<'s,'M,'a> =
            StateT.fromFunction <| fun state -> innerMonad.Pure (state, x) 

        member _.Apply (fab: StateT<'s,'M,'a -> 'b>) (a: StateT<'s,'M,'a>) : StateT<'s,'M, 'b> =
            StateT.fromFunction <| fun state -> 
                monad innerMonad {
                    let! (state1, f) = StateT.run state fab
                    let! (state2, a) = StateT.run state1 a

                    return (state2, f a)
                }

        member _.Bind (ma: StateT<'s,'M,'a>) (f : 'a -> StateT<'s,'M,'b>): StateT<'s,'M,'b> =
            StateT.fromFunction <| fun state ->
                monad innerMonad {
                    let! (state1, a) = StateT.run state ma
                    return! f a |> StateT.run state1
                }

    interface MonadTrans<StateTH<'s>,'M, 'MI> with
        member _.Lift (app: App<'M,'a>) : StateT<'s, 'M, 'a> =
            StateT.fromFunction <| fun st -> innerMonad.Map (fun a -> (st, a)) app

        member _.InnerMonad = innerMonad

    static member Instance<'s> monad = StateTMonad(monad) :> Monad<App<StateTH<'s>,'M>>

let stateT (inner: MonadBuilder<'M, 'S>) = monadT <| StateTMonad (inner)

type State<'s,'a> = StateT<'s, IdentityH, 'a>

module State =
    let get<'s> = StateT.get<_, 's> IdentityMonad.Instance 

    let put (state: 'state) = StateT.put IdentityMonad.Instance state

    let run (state: 'state) = StateT.run state >> Identity.run

type StateMonad<'s> () = 
    inherit StateTMonad<'s, IdentityH, IdentityMonad>(IdentityMonad.Instance)
    
    static member Instance = StateMonad<'s>()

let state<'s> = monad <| StateMonad<'s>()