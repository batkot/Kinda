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

let monad monad = MonadBuilder(monad)

type MonadTrans<'T, 'M> = 
    inherit Monad<App<'T, 'M>>

    abstract Lift: App<'M, 'a> -> App<App<'T,'M>, 'a>

type MonadTransBuilder<'T, 'M, 'S when 'S :> MonadTrans<'T, 'M>> (trans: 'S) =
    inherit MonadBuilder<App<'T,'M>, 'S> (trans)

    member _.Lift m = trans.Lift m

let monadT monadTrans = MonadTransBuilder(monadTrans)