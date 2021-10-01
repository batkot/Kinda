module Kindly.Monad

open Kindly.App
open Kindly.Applicative

type Monad<'M> = 
    inherit Applicative<'M>
    abstract Bind: App<'M, 'a> -> ('a -> App<'M, 'b>) -> App<'M, 'b>

module Monad = 
    let flipBind (monad: Monad<'M>) f x = monad.Bind x f