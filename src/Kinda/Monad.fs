module Kinda.Monad

open Kinda.App
open Kinda.Applicative

type Monad<'M> = 
    inherit Applicative<'M>
    abstract Bind: App<'M, 'a> -> ('a -> App<'M, 'b>) -> App<'M, 'b>

module Monad = 
    let flipBind (monad: Monad<'M>) f x = monad.Bind x f

type MonadBuilder<'M,'S when 'S:> Monad<'M>> (monad: 'S) =
    member _.Monad = monad
    member _.Return x = monad.Pure x
    member _.ReturnFrom x = x
    member _.Bind (x, f) = monad.Bind x f

    interface Monad<'M> with
        member _.Map f x = monad.Map f x
        member _.Pure x = monad.Pure x
        member _.Apply f x = monad.Apply f x
        member _.Bind x f = monad.Bind x f

let monad monad = MonadBuilder(monad)

type MonadTrans<'T, 'M, 'S when 'S :> Monad<'M>> = 
    inherit Monad<App<'T, 'M>>

    abstract Lift: App<'M, 'a> -> App<App<'T,'M>, 'a>
    abstract InnerMonad: 'S

type MonadTransBuilder<'T, 'M, 'S, 'G when 'G :> Monad<'M> and 'S :> MonadTrans<'T, 'M, 'G>> (trans: 'S) =
    inherit MonadBuilder<App<'T,'M>, 'S> (trans)

    interface MonadTrans<'T, 'M, 'G> with
        member _.Lift m = trans.Lift m
        member _.InnerMonad = trans.InnerMonad

let monadT monadTrans = MonadTransBuilder(monadTrans)

module MonadTrans =
    let innerMonad (trans: MonadTrans<'T, 'M, 'S>) = trans.InnerMonad
    let innerMonad2 trans = innerMonad trans |> innerMonad
    let innerMonad3 trans = innerMonad2 trans |> innerMonad
    let innerMonad4 trans = innerMonad3 trans |> innerMonad

    let lift (trans: MonadTrans<'T, 'M, 'S>) = trans.Lift
    let lift2 trans = innerMonad trans |> lift >> lift trans
    let lift3 trans = innerMonad trans |> lift2 >> lift trans
    let lift4 trans = innerMonad trans |> lift3 >> lift trans
