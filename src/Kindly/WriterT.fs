module Kindly.WriterT

open Kindly.App
open Kindly.Identity
open Kindly.Monad

type Monoid<'m> = 
    abstract Mempty: 'm
    abstract Combine: 'm -> 'm -> 'm

type WriterT<'w, 'M, 'a> = WriterT of App<'M, 'a * 'w>

let runWriterT (WriterT x) = x

module WriterT = 
    let tell (innerMonad: Monad<'M>) (writer: 'w) = WriterT <| innerMonad.Pure ((), writer)

type WriterTH<'w, 'M> = 
    static member Inject (writer: WriterT<'w, 'M,'a>) : App<WriterTH<'w,'M>, 'a> =
        create writer
    static member Project (app: App<WriterTH<'w,'M>,'a>) : WriterT<'w, 'M, 'a> = 
        unwrap app :?> _

//Tmp
type ListMonoid<'a> () =
    interface Monoid<'a list> with
        member _.Mempty = []
        member _.Combine x y = x @ y

type WriterTMonad<'w, 'M> (writerMonoid: Monoid<'w>, innerMonad: Monad<'M>) =
    interface Monad<WriterTH<'w,'M>> with
        member _.Map (f : 'a -> 'b) (x: App<WriterTH<'w, 'M>, 'a>) : App<WriterTH<'w,'M>,'b> =
            WriterTH.Project x 
            |> runWriterT
            |> innerMonad.Map (fun (a, w) -> (f a , w))
            |> WriterT
            |> WriterTH.Inject

        member _.Pure (x : 'a) : App<WriterTH<'w,'M>, 'a> = 
            innerMonad.Pure (x, writerMonoid.Mempty)
            |> WriterT 
            |> WriterTH.Inject

        member _.Apply (fab: App<WriterTH<'w,'M>, 'a -> 'b>) (a: App<WriterTH<'w,'M>,'a>) : App<WriterTH<'w,'M>, 'b> = 
            monad innerMonad {
                let! (f, w1) = fab |> WriterTH.Project |> runWriterT
                let! (a, w2) = a |> WriterTH.Project |> runWriterT

                return (f a, writerMonoid.Combine w1 w2)
            } |> WriterT |> WriterTH.Inject

        member _.Bind (ma: App<WriterTH<'w,'M>, 'a>) (f : 'a -> App<WriterTH<'w, 'M>, 'b>) =
            monad innerMonad {
                let! (a, w1) = ma |> WriterTH.Project |> runWriterT
                let! (b, w2) = f a |> WriterTH.Project |> runWriterT
                return (b, writerMonoid.Combine w1 w2)
            } |> WriterT |> WriterTH.Inject

    static member Instance monoid innerMonad = WriterTMonad(monoid, innerMonad) :> WriterTMonad<'w, 'M>

type Writer<'w, 'a> = WriterT<'w, Identity, 'a>

module Writer =
    let tell (w: 'w) = WriterT.tell IdentityMonad.Instance w

type WriterMonad<'w>(writerMonoid: Monoid<'w>) = 
    inherit WriterTMonad<'w, Identity>(writerMonoid, IdentityMonad.Instance)

    static member MonadInstance monoid = WriterMonad(monoid) :> Monad<WriterTH<'w, Identity>>