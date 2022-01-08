module Kinda.WriterT

open Kinda.App
open Kinda.Identity
open Kinda.Monad
open Kinda.Monoid
open Kinda.Void

type private InnerWriterT<'w, 'M, 'a> = MkWriterT of App<'M, 'a * 'w>

let private runWriterT (MkWriterT x) = x
let private tell (innerMonad: Monad<'M>) (writer: 'w) = MkWriterT <| innerMonad.Pure ((), writer)

type WriterTH<'w> = private WTH of Void
type WriterTH<'w, 'M> = App<WriterTH<'w>, 'M>

type WriterT<'w,'M,'a> = App<WriterTH<'w,'M>, 'a>

module WriterT = 
    let private inject (writer: InnerWriterT<'w, 'M,'a>) : WriterT<'w,'M, 'a> =
        create writer

    let private project (writer: WriterT<'w, 'M, 'a>) : InnerWriterT<'w, 'M, 'a> =
        unwrap writer :?> _

    let run (writer: WriterT<'w, 'M, 'a>) =
        project writer |> runWriterT

    let tell (innerMonad: Monad<'M>) (w: 'w) : WriterT<'w,'M, unit> =
        tell innerMonad w |> inject

    let fromApp (app: App<'M, 'a * 'w>) =
        MkWriterT app |> inject

type WriterTMonad<'w, 'M, 'MI when 'MI :> Monad<'M>> (writerMonoid: Monoid<'w>, innerMonad: 'MI) =
    interface Monad<WriterTH<'w,'M>> with
        member _.Map (f : 'a -> 'b) (x: WriterT<'w, 'M, 'a>) : WriterT<'w,'M,'b> =
            WriterT.run x
            |> innerMonad.Map (fun (a, w) -> (f a , w))
            |> WriterT.fromApp

        member _.Pure (x : 'a) : WriterT<'w,'M, 'a> = 
            innerMonad.Pure (x, writerMonoid.Empty)
            |> WriterT.fromApp

        member _.Apply (fab: WriterT<'w,'M, 'a -> 'b>) (a: WriterT<'w,'M,'a>) : WriterT<'w,'M, 'b> = 
            monad innerMonad {
                let! (f, w1) = fab |> WriterT.run
                let! (a, w2) = a |> WriterT.run

                return (f a, writerMonoid.Combine w1 w2)
            } |> WriterT.fromApp

        member _.Bind (ma: WriterT<'w,'M, 'a>) (f : 'a -> WriterT<'w, 'M, 'b>) =
            monad innerMonad {
                let! (a, w1) = ma |> WriterT.run
                let! (b, w2) = f a |> WriterT.run
                return (b, writerMonoid.Combine w1 w2)
            } |> WriterT.fromApp

    interface MonadTrans<WriterTH<'w>, 'M, 'MI> with
        member _.Lift ma = 
            innerMonad.Map (fun a -> (a, writerMonoid.Empty)) ma
            |> WriterT.fromApp

        member _.InnerMonad = innerMonad

    static member Instance monoid innerMonad = WriterTMonad(monoid, innerMonad) :> WriterTMonad<'w, 'M, 'MI>

type WriterH<'w> = WriterTH<'w, IdentityH>
type Writer<'w, 'a> = WriterT<'w, IdentityH, 'a>

let writerT writerMonoid (inner: MonadBuilder<'M,'S>) = monadT <| WriterTMonad (writerMonoid, inner)

module Writer =
    let tell (w: 'w) : Writer<'w, unit> = 
        WriterT.tell IdentityMonad.Instance w

    let fromTuple (x: 'a * 'w) : Writer<'w,'a> =
        Identity.fromA x
        |> WriterT.fromApp

type WriterMonad<'w>(writerMonoid: Monoid<'w>) = 
    inherit WriterTMonad<'w, IdentityH, IdentityMonad>(writerMonoid, IdentityMonad.Instance)

    static member MonadInstance monoid = WriterMonad(monoid)

let writer monoid = monad <| WriterMonad<'w>(monoid)