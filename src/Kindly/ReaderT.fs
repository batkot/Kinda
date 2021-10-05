module Kindly.ReaderT

open Kindly.App
open Kindly.Monad
open Kindly.Identity
open Kindly.Void

type private InnerReaderT<'r,'M,'a> = MkReaderT of ('r -> App<'M, 'a>)

let private runReaderT (env: 'r) (MkReaderT reader) = reader env
let private ask (innerMonad: Monad<'M>) = MkReaderT <| innerMonad.Pure 

type ReaderTH<'r> = private RTH of Void
type ReaderT<'r,'M, 'a> = App<App<ReaderTH<'r>, 'M>, 'a>

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


type ReaderTMonad<'r, 'M, 'I when 'I :> Monad<'M>> (innerMonad: 'I) =

    member _.InnerMonad = innerMonad
    member self.Lift x = (self :> MonadTrans<_,_>).Lift x

    interface Monad<App<ReaderTH<'r>,'M>> with
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

    interface MonadTrans<ReaderTH<'r>,'M> with
        member _.Lift (ma: App<'M,'a>): ReaderT<'r,'M,'a> = 
            ReaderT.fromFunction <| fun _ -> ma

    static member Instance innerMonad = ReaderTMonad(innerMonad) :> ReaderTMonad<'r,'M, 'I>

type Reader<'r,'a> = ReaderT<'r, Identity, 'a>

type ReaderMonad<'r> () =
    inherit ReaderTMonad<'r, Identity, IdentityMonad>(IdentityMonad.Instance)

    static member Instance = ReaderMonad() :> ReaderTMonad<'r,_, _>

module Reader = 
    let ask<'r> = ReaderT.ask IdentityMonad.Instance : Reader<'r, 'r>
    let run env = ReaderT.run env >> Identity.Run