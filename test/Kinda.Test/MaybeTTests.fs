module Kinda.Test.MaybeTTests

open Expecto
open FsCheck
open Kinda.Test.Helpers

open Kinda.MaybeT
open Kinda.Monad

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

let maybeMonad = MaybeMonad.Instance :> Monad<_>
type MaybeGen = 
    static member Generator () : Arbitrary<HktGen<MaybeH>> =
        { new HktGen<MaybeH> with 
            member _.Generate<'a> () =
                gen {
                    let! x = Arb.generate<'a>
                    let! isJust = Arb.generate<bool>
                    return 
                        if isJust 
                        then Maybe.just x
                        else Maybe.nothing
                }
        } |> HktGen.toArb

let fsCheckConfig = FsCheckConfig.withFunctorGen<MaybeGen>

[<Tests>]
let tests = 
    testList "Maybe Monad Tests" [
        functorLaws fsCheckConfig defaultEquality maybe
        applicativeLaws fsCheckConfig defaultEquality maybe
        monadLaws fsCheckConfig defaultEquality maybe

        let increment x = maybe { return x + 1 }
        testProperty "When no Nothings should return computation result" <| fun (x: int) (y: int) -> 
            let numberOfActions = ((abs x) % 1000)
            let combinedAction =
                List.replicate numberOfActions increment
                |> List.fold maybeMonad.Bind (maybeMonad.Pure x)

            Some (numberOfActions + x) = Maybe.run combinedAction

        testProperty "When Nothing should return Nothing" <| fun (x: int) (y: int) ->
            let numberOfActions = ((abs x) % 1000) + 1
            let failAt = (abs y) % numberOfActions
            let shouldRun, shouldNotRun =
                List.replicate numberOfActions increment
                |> List.splitAt failAt

            let result = 
                shouldRun @ [fun _ -> Maybe.nothing] @ shouldNotRun
                |> List.fold maybeMonad.Bind (maybeMonad.Pure 0)
                |> Maybe.run

            None = result
    ]