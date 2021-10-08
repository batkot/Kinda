module Kinda.ExceptT

open Kinda.App
open Kinda.Monad
open Kinda.Void

open Kinda.Identity

type Either<'a,'b>
    = Left of 'a
    | Right of 'b

module Either =
    let bimap f g = function
        | Left x -> Left <| f x
        | Right y -> Right <| g y

    let mapLeft f = bimap f id

    let mapRight f = bimap id f

    let either f g = function
        | Left x -> f x
        | Right y -> g y

    let collapse x = either id id x

    let collapseLeft f = either f id

    let collapseRight f = either id f
    
type private InnerExceptT<'e, 'M, 'a> = App<'M, Either<'e, 'a>>

type ExceptTH<'e> = private ETH of Void
type ExceptTH<'e,'M> = App<ExceptTH<'e>, 'M>

type ExceptT<'e, 'M, 'a> = App<ExceptTH<'e,'M>, 'a>

module private ExceptTH =
    let inject (innerExcept: InnerExceptT<'e, 'M, 'a>) : ExceptT<'e, 'M, 'a> =
        create innerExcept
    let project (except: ExceptT<'e,'M,'a>): InnerExceptT<'e, 'M, 'a> =
        unwrap except :?> _

module ExceptT =
    let run (except: ExceptT<'e, 'M, 'a>) : App<'M, Either<'e,'a>> =
        ExceptTH.project except

    let fromApp (app: App<'M, Either<'e,'a>>) : ExceptT<'e, 'M, 'a> =
        ExceptTH.inject app

    let fromEither (innerMonad: Monad<'M>) (either: Either<'e, 'a>) : ExceptT<'e,'M, 'a> = 
        innerMonad.Pure either
        |> fromApp

    let throwError (innerMonad: Monad<'M>) (error: 'e) : ExceptT<'e, 'M, 'a> = 
        Left error
        |> fromEither innerMonad


type ExceptTMonad<'e, 'M, 'MI when 'MI :> Monad<'M>> (innerMonad: 'MI) =

    interface Monad<ExceptTH<'e,'M>> with
        member _.Map (f : 'a -> 'b) (x: ExceptT<'e, 'M, 'a>) : ExceptT<'e,'M,'b> = 
            ExceptT.run x
            |> innerMonad.Map (Either.mapRight f)
            |> ExceptT.fromApp

        member _.Pure (x: 'a) : ExceptT<'e, 'M, 'a> =
            Right x
            |> ExceptT.fromEither innerMonad

        member _.Apply (fab: ExceptT<'e,'M,'a -> 'b>) (fa: ExceptT<'e,'M,'a>) : ExceptT<'e,'M,'b> =
            monad innerMonad {
                let! f = ExceptT.run fab
                let! a = ExceptT.run fa

                return
                    match f, a with
                    | Left err, _ -> Left err
                    | _, Left err -> Left err
                    | Right f, Right a -> Right <| f a
            } |> ExceptT.fromApp

        member _.Bind (ma: ExceptT<'e,'M,'a>) (f: 'a -> ExceptT<'e, 'M, 'b>) : ExceptT<'e,'M,'b> =
            monad innerMonad {
                let! a = ExceptT.run ma

                return! 
                    match a with
                    | Left err -> Left err |> innerMonad.Pure
                    | Right x -> f x |> ExceptT.run
            } |> ExceptT.fromApp

    interface MonadTrans<ExceptTH<'e>, 'M, 'MI> with
        member _.Lift (app: App<'M,'a>) : ExceptT<'e, 'M, 'a> =
            app |> innerMonad.Map Right |> ExceptT.fromApp

        member _.InnerMonad = innerMonad

let exceptT (innerMonad: MonadBuilder<'M, 'MI>) = monadT <| ExceptTMonad(innerMonad)

type Except<'e> = ExceptT<'e, IdentityH, IdentityMonad>

module Except =
    let throwError err = ExceptT.throwError IdentityMonad.Instance err
    let run x = ExceptT.run x |> Identity.run

type ExceptMonad<'e>() = 
    inherit ExceptTMonad<'e, IdentityH, IdentityMonad> (IdentityMonad.Instance)

    static member Instance = ExceptMonad() :> ExceptMonad<'e>

let except<'e> = monad <| ExceptMonad<'e>()