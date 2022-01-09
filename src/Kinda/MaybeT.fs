module Kinda.MaybeT

open Kinda.App
open Kinda.Monad
open Kinda.Void

open Kinda.Identity

type private InnerMaybeT<'M, 'a> = MkMaybeT of App<'M, 'a option>
module private InnerMaybeT = 
    let deconstruct (MkMaybeT x) = x

type MaybeTH = private MH of Void
type MaybeTH<'M> = App<MaybeTH, 'M>
type MaybeT<'M, 'a> = App<MaybeTH<'M>, 'a>

module MaybeT = 
    let private inject (innerMaybe: InnerMaybeT<'M, 'a>) : MaybeT<'M, 'a> = 
        create innerMaybe

    let private project (maybe: MaybeT<'M, 'a>) : InnerMaybeT<'M,'a> = 
        unwrap maybe :?> _

    let run (maybe: MaybeT<'M, 'a>) : App<'M, 'a option> = 
        project maybe |> InnerMaybeT.deconstruct

    let fromOption (innerMonad: Monad<'M>) (x: 'a option) = 
        innerMonad.Pure x |> MkMaybeT |> inject

    let fromApp (app: App<'M, option<'a>>) =
        MkMaybeT app |> inject

    let just (innerMonad: Monad<'M>) (x: 'a) : MaybeT<'M,'a> = 
        fromOption innerMonad <| Some x

    let nothing (innerMonad: Monad<'M>) : MaybeT<'M,'a> =
        fromOption innerMonad <| None

type MaybeTMonad<'M, 'MI when 'MI :> Monad<'M>> (innerMonad: 'MI) = 
    interface Monad<MaybeTH<'M>> with
        member _.Map (f: 'a -> 'b) (x: MaybeT<'M, 'a>) : MaybeT<'M, 'b> = 
            MaybeT.run x 
            |> innerMonad.Map (Option.map f) 
            |> MaybeT.fromApp

        member _.Pure (x: 'a) : MaybeT<'M, 'a> = 
            MaybeT.just innerMonad x

        member _.Apply (f: MaybeT<'M, 'a -> 'b>) (x: MaybeT<'M, 'a>) : MaybeT<'M, 'b> = 
            monad innerMonad {
                let! optF = MaybeT.run f
                let! optX = MaybeT.run x

                return 
                    match optF, optX with
                    | Some f, Some x -> Some <| f x
                    | _, _ -> None
            } |> MaybeT.fromApp

        member _.Bind (x: MaybeT<'M, 'a>) (f: 'a -> MaybeT<'M, 'b>) : MaybeT<'M, 'b> = 
            monad innerMonad {
                let! optX = MaybeT.run x
                return!
                    match Option.map f optX with
                    | Some x -> MaybeT.run x
                    | None -> innerMonad.Pure None
            } |> MaybeT.fromApp

    interface MonadTrans<MaybeTH,'M, 'MI> with
        member _.Lift (app: App<'M,'a>) : MaybeT<'M, 'a> =
            innerMonad.Map Some app
            |> MaybeT.fromApp

        member _.InnerMonad = innerMonad

let maybeT (inner: MonadBuilder<'M, 'S>) = monadT <| MaybeTMonad (inner.Monad)

type MaybeH = MaybeTH<IdentityH>
type Maybe<'a> = MaybeT<IdentityH, 'a>

type MaybeMonad() = 
    inherit MaybeTMonad<IdentityH,IdentityMonad>(IdentityMonad.Instance)

    static member Instance = MaybeMonad()

module Maybe = 
    let run<'a> : Maybe<'a> -> 'a option = 
        MaybeT.run >> Identity.run

    let just<'a> (x: 'a) : Maybe<'a> = MaybeT.just IdentityMonad.Instance x

    let nothing<'a> : Maybe<'a> = MaybeT.nothing IdentityMonad.Instance

let maybe = monad <| MaybeMonad()