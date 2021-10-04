module Kindly.Monad

open Kindly.App
open Kindly.Applicative

type Monad<'M> = 
    inherit Applicative<'M>
    abstract Bind: App<'M, 'a> -> ('a -> App<'M, 'b>) -> App<'M, 'b>

module Monad = 
    let flipBind (monad: Monad<'M>) f x = monad.Bind x f

type MonadBuilder<'M> (monad: Monad<'M>) =
    member _.Return x = monad.Pure x
    member _.ReturnFrom x = x
    member _.Bind (x, f) = monad.Bind x f

let monad (monad: Monad<'M>) = MonadBuilder(monad)

type MonadTrans<'T, 'M> = 
    abstract Lift: App<'M, 'a> -> App<App<'T,'M>, 'a>