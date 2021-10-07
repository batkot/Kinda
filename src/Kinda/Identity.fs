module Kinda.Identity

open Kinda.App
open Kinda.Monad
open Kinda.Void

type Identity<'a> = Id of 'a

let runIdentity (Id a) = a
let map f (Id a) = Id <| f a
let retn = Id
let apply (Id f) x = map f x
let bind (Id a) f = f a

type IdentityH = private IdH of Void

module Identity =
    let inject (identity: Identity<'a>) : App<IdentityH, 'a> =
        create identity
    let project (app: App<IdentityH, 'a>) : Identity<'a> = 
        unwrap app :?> _
    let run (app: App<IdentityH, 'a>) : 'a = 
        project app |> runIdentity
    let fromA (x: 'a): App<IdentityH,'a> = retn x |> inject

type IdentityMonad () =
    interface Monad<IdentityH> with
        member _.Map (f : 'a -> 'b) (x: App<IdentityH, 'a>) : App<IdentityH,'b> =
            Identity.project x |> map f |> Identity.inject

        member _.Pure (x : 'a) : App<IdentityH, 'a> = 
            Identity.fromA x

        member _.Apply (fab: App<IdentityH, 'a -> 'b>) (a: App<IdentityH,'a>) : App<IdentityH, 'b> = 
            apply (Identity.project fab) (Identity.project a)
            |> Identity.inject

        member _.Bind (ma: App<IdentityH, 'a>) (f : 'a -> App<IdentityH, 'b>) =
            Identity.run ma |> f

    static member Instance = IdentityMonad ()

let identity = monad <| IdentityMonad ()