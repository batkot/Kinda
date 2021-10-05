module Kinda.Applicative

open Kinda.App
open Kinda.Functor

type Applicative<'F> = 
    inherit Functor<'F>

    abstract Pure: 'a -> App<'F,'a>
    abstract Apply: App<'F, 'a -> 'b> -> App<'F, 'a> -> App<'F, 'b>
