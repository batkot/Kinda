module Kindly.Free

open Kindly.App
open Kindly.Functor
open Kindly.Monad

type Free<'F, 'a> =
    | Pure of 'a
    | Free of App<'F, Free<'F,'a>>

type FreeH<'F> = 
    static member Inject (free: Free<'F, 'a>) : App<FreeH<'F>, 'a> =
        create free
    static member Project (app: App<FreeH<'F>, 'a>) : Free<'F, 'a> = 
        unwrap app :?> _

type FreeMonad<'F> (innerFunctor: Functor<'F>) =
    interface Monad<FreeH<'F>> with
        member _.Map (f : 'a -> 'b) (x: App<FreeH<'F>, 'a>) : App<FreeH<'F>,'b> =
            let rec map f x =
                let x =
                    match FreeH.Project x with
                    | Pure x -> Pure (f x)
                    | Free fa -> innerFunctor.Map (FreeH.Inject >> map f >> FreeH.Project) fa |> Free

                FreeH.Inject x

            map f x

        member _.Pure (x : 'a) : App<FreeH<'F>, 'a> = 
            Pure x |> FreeH.Inject

        member self.Apply (fab: App<FreeH<'F>, 'a -> 'b>) (a: App<FreeH<'F>,'a>) : App<FreeH<'F>, 'b> = 
            let rec apply fab a = 
                match FreeH.Project fab with
                | Pure f -> (self :> Monad<FreeH<'F>>).Map f a
                | Free fa -> 
                    innerFunctor.Map (fun f -> apply (FreeH.Inject f) a |> FreeH.Project) fa
                    |> Free
                    |> FreeH.Inject

            apply fab a

        member _.Bind (ma: App<FreeH<'F>, 'a>) (f : 'a -> App<FreeH<'F>, 'b>) =
            let rec bind ma f =
                match FreeH.Project ma with
                | Pure a -> f a
                | Free fa -> 
                    innerFunctor.Map (fun a -> bind (FreeH.Inject a) f |> FreeH.Project) fa
                    |> Free
                    |> FreeH.Inject

            bind ma f

type NaturalTransform<'F,'M> = 
    abstract Transform : App<'F, 'a> -> App<'M, 'a>

let rec runFree 
    (m: Monad<'M>)
    (naturalTransform: NaturalTransform<'F, 'M>)
    (free: Free<'F,'a>)
    : App<'M, 'a> =
        match free with
        | Pure x -> m.Pure x
        | Free f -> 
            naturalTransform.Transform f
            |> Monad.flipBind m (runFree m naturalTransform)