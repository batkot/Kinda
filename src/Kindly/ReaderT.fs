module Kindly.ReaderT

open Kindly.App
open Kindly.Monad
open Kindly.Identity

type ReaderT<'r,'m,'a> = ReaderT of ('r -> App<'m, 'a>)

let runReaderT (env: 'r) (ReaderT reader) = reader env

module ReaderT =
    let ask (innerMonad: Monad<'M>) = ReaderT <| innerMonad.Pure 

type ReaderTH<'r, 'M> = 
    static member Inject (reader: ReaderT<'r, 'M, 'a>) : App<ReaderTH<'r,'M>, 'a> =
        create reader
    static member Project (app: App<ReaderTH<'r, 'M>, 'a>) : ReaderT<'r, 'M, 'a> = 
        unwrap app :?> _

    static member Run (env: 'r) (app: App<ReaderTH<'r,'M>, 'a>) = 
        ReaderTH.Project app |> runReaderT env

type ReaderTMonad<'r, 'M> (innerMonad: Monad<'M>) =
    interface Monad<ReaderTH<'r,'M>> with
        member _.Map (f: 'a -> 'b) (x: App<ReaderTH<'r,'M>, 'a>) : App<ReaderTH<'r,'M>,'b> =
            ReaderT <| fun env -> 
                ReaderTH.Project x
                |> runReaderT env
                |> innerMonad.Map f
            |> ReaderTH.Inject

        member _.Pure (x: 'a) : App<ReaderTH<'r, 'M>, 'a> =
            ReaderT <| fun _ -> innerMonad.Pure(x)
            |> ReaderTH.Inject

        member _.Apply (fab: App<ReaderTH<'r,'M>, 'a -> 'b>) (fa : App<ReaderTH<'r,'M>, 'a>) : App<ReaderTH<'r,'M>, 'b> =
            ReaderT <| fun env -> 
                monad innerMonad {
                    let! f = ReaderTH.Project fab |> runReaderT env
                    let! a = ReaderTH.Project fa |> runReaderT env
                    return f a
                }
            |> ReaderTH.Inject

        member _.Bind (ma: App<ReaderTH<'r,'M>, 'a>) (f: 'a -> App<ReaderTH<'r,'M>, 'b>) : App<ReaderTH<'r,'M>, 'b> =
            ReaderT <| fun env ->
                monad innerMonad {
                    let! a = ReaderTH.Project ma |> runReaderT env
                    return! f a |> ReaderTH.Project |> runReaderT env
                }
            |> ReaderTH.Inject

    static member Instance innerMonad = ReaderTMonad(innerMonad) :> ReaderTMonad<'r,'M>

type Reader<'r,'a> = ReaderT<'r, Identity, 'a>

let runReader env = runReaderT env >> Identity.Project >> runIdentity

type ReaderMonad<'r> () =
    inherit ReaderTMonad<'r, Identity>(IdentityMonad.Instance)

    static member Instance = ReaderMonad() :> ReaderTMonad<'r,Identity>

module Reader = 
    let ask<'r> = ReaderT.ask IdentityMonad.Instance : Reader<'r, 'r>