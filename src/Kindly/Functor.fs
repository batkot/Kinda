module Kindly.Functor

open Kindly.App

type Functor<'F> = 
    abstract Map : ('a -> 'b) -> App<'F, 'a> -> App<'F,'b>
