module Kinda.ReaderT

open Kinda.App
open Kinda.Monad
open Kinda.Identity
open Kinda.Void

type private InnerReaderT<'r,'M,'a> = MkReaderT of ('r -> App<'M, 'a>)

let private runReaderT (env: 'r) (MkReaderT reader) = reader env
let private ask (innerMonad: Monad<'M>) = MkReaderT <| innerMonad.Pure 

type ReaderTH<'r> = private RTH of Void
type ReaderTH<'r,'M> = App<ReaderTH<'r>, 'M>
type ReaderT<'r,'M, 'a> = App<ReaderTH<'r, 'M>, 'a>

module ReaderT =
    let private inject (reader: InnerReaderT<'r,'M,'a>) : ReaderT<'r,'M,'a> = 
        create reader

    let private project (app: ReaderT<'r,'M,'a>): InnerReaderT<'r, 'M, 'a> =
        unwrap app :?> _

    let ask (innerMonad: Monad<'M>): ReaderT<'r, 'M, 'r> =
        ask innerMonad |> inject

    let run (env: 'r) (x: ReaderT<'r,'M,'a>) =
        project x |> runReaderT env

    let fromFunction (readerF: 'r -> App<'M, 'a>) =
        MkReaderT readerF |> inject


type ReaderTMonad<'r, 'M, 'MI when 'MI :> Monad<'M>> (innerMonad: 'MI) =

    interface Monad<ReaderTH<'r,'M>> with
        member _.Map (f: 'a -> 'b) (x: ReaderT<'r,'M,'a>) : ReaderT<'r,'M,'b> =
            ReaderT.fromFunction <| fun env -> 
                ReaderT.run env x
                |> innerMonad.Map f

        member _.Pure (x: 'a) : ReaderT<'r,'M,'a> =
            ReaderT.fromFunction <| fun _ -> innerMonad.Pure(x)

        member _.Apply (fab: ReaderT<'r,'M,'a -> 'b>) (fa : ReaderT<'r,'M, 'a>) : ReaderT<'r,'M,'b> =
            ReaderT.fromFunction <| fun env -> 
                monad innerMonad {
                    let! f = ReaderT.run env fab
                    let! a = ReaderT.run env fa
                    return f a
                }

        member _.Bind (ma: ReaderT<'r,'M,'a>) (f: 'a -> ReaderT<'r,'M,'b>) : ReaderT<'r,'M,'b> =
            ReaderT.fromFunction <| fun env ->
                monad innerMonad {
                    let! a = ReaderT.run env ma
                    return! f a |> ReaderT.run env
                }

    interface MonadTrans<ReaderTH<'r>,'M, 'MI> with
        member _.Lift (ma: App<'M,'a>): ReaderT<'r,'M,'a> = 
            ReaderT.fromFunction <| fun _ -> ma

        member _.InnerMonad = innerMonad

    static member Instance innerMonad = ReaderTMonad(innerMonad) :> ReaderTMonad<'r,'M, 'I>

let readerT (inner: MonadBuilder<'M, 'I>) = monadT <|  ReaderTMonad(inner.Monad)

type ReaderH<'r> = ReaderTH<'r, IdentityH>
type Reader<'r,'a> = ReaderT<'r, IdentityH, 'a>

type ReaderMonad<'r> () =
    inherit ReaderTMonad<'r, IdentityH, IdentityMonad>(IdentityMonad.Instance)

    static member Instance = ReaderMonad() :> ReaderTMonad<'r,_, _>

module Reader = 
    let ask<'r> : Reader<'r,'r> = ReaderT.ask IdentityMonad.Instance
    let run env = ReaderT.run env >> Identity.run
    let fromFunction (f: 'r -> 'a) =
        f >> Identity.fromA
        |> ReaderT.fromFunction

let reader<'r> = monad <| ReaderMonad<'r>()