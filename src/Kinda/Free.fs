module Kinda.Free

open Kinda.App
open Kinda.Functor
open Kinda.Monad
open Kinda.Void

type FreeF<'F, 'a> =
    | Pure of 'a
    | Free of App<'F, FreeF<'F,'a>>

type FreeH<'F> = private FH of Void

type Free<'F,'a> = App<FreeH<'F>, 'a>

module private Free =
    let inject (free: FreeF<'F,'a>) : Free<'F,'a> =
        create free
    let project (free: Free<'F,'a>) : FreeF<'F,'a> = 
        unwrap free :?> _

type FreeMonad<'F> (innerFunctor: Functor<'F>) =
    interface Monad<FreeH<'F>> with
        member _.Map (f : 'a -> 'b) (x: Free<'F, 'a>) : Free<'F,'b> =
            let rec map f x =
                let x =
                    match Free.project x with
                    | Pure x -> Pure (f x)
                    | Free fa -> innerFunctor.Map (Free.inject >> map f >> Free.project) fa |> Free

                Free.inject x

            map f x

        member _.Pure (x : 'a) : Free<'F, 'a> = 
            Pure x |> Free.inject

        member self.Apply (fab: Free<'F, 'a -> 'b>) (a: Free<'F,'a>) : Free<'F, 'b> = 
            let rec apply fab a = 
                match Free.project fab with
                | Pure f -> (self :> Monad<FreeH<'F>>).Map f a
                | Free fa -> 
                    innerFunctor.Map (fun f -> apply (Free.inject f) a |> Free.project) fa
                    |> Free
                    |> Free.inject

            apply fab a

        member _.Bind (ma: Free<'F, 'a>) (f : 'a -> Free<'F, 'b>) =
            let rec bind ma f =
                match Free.project ma with
                | Pure a -> f a
                | Free fa -> 
                    innerFunctor.Map (fun a -> bind (Free.inject a) f |> Free.project) fa
                    |> Free
                    |> Free.inject

            bind ma f

type NaturalTransformation<'F,'M> = 
    abstract Transform : App<'F, 'a> -> App<'M, 'a>

let liftF (functor: Functor<'F>) (f: App<'F,'a>): Free<'F,'a> =
    functor.Map Pure f 
    |> Free
    |> Free.inject

let rec runFree 
    (m: Monad<'M>)
    (nt: NaturalTransformation<'F, 'M>)
    (free: Free<'F,'a>)
    : App<'M, 'a> =
        match Free.project free with
        | Pure x -> m.Pure x
        | Free f -> 
            nt.Transform f
            |> Monad.flipBind m (Free.inject >> runFree m nt)