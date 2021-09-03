module Kindly.Applicative

open Kindly.App
open Kindly.Functor

type Applicative<'F> = 
    inherit Functor<'F>

    abstract Pure: 'a -> App<'F,'a>
    abstract Apply: App<'F, 'a -> 'b> -> App<'F, 'a> -> App<'F, 'b>
