module Kindly.Identity

open Kindly.App
open Kindly.Monad

type Identity<'a> = Id of 'a

let runIdentity (Id a) = a
let map f (Id a) = Id <| f a
let retn = Id
let apply (Id f) x = map f x
let bind (Id a) f = f a

type Identity =
    static member Inject (identity: Identity<'a>) : App<Identity, 'a> =
        create identity
    static member Project (app: App<Identity, 'a>) : Identity<'a> = 
        unwrap app :?> _

type IdentityMonad () =
    interface Monad<Identity> with
        member _.Map (f : 'a -> 'b) (x: App<Identity, 'a>) : App<Identity,'b> =
            Identity.Project x |> map f |> Identity.Inject

        member _.Pure (x : 'a) : App<Identity, 'a> = 
            retn x |> Identity.Inject

        member _.Apply (fab: App<Identity, 'a -> 'b>) (a: App<Identity,'a>) : App<Identity, 'b> = 
            apply (Identity.Project fab) (Identity.Project a)
            |> Identity.Inject

        member _.Bind (ma: App<Identity, 'a>) (f : 'a -> App<Identity, 'b>) =
            Identity.Project ma
            |> (runIdentity >> f)

    static member Instance = IdentityMonad () :> Monad<Identity>

