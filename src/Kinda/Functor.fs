module Kinda.Functor

open Kinda.App

type Functor<'F> = 
    abstract Map : ('a -> 'b) -> App<'F, 'a> -> App<'F,'b>
