module Kinda.ExceptTTests

open Expecto

open FsCheck

open Kinda.Monad
open Kinda.StateT
open Kinda.ExceptT

open Kinda.Test.FunctorTests
open Kinda.Test.ApplicativeTests
open Kinda.Test.MonadTests

type TestError = private Error of string

let exceptMonad = ExceptMonad.Instance :> ExceptMonad<TestError>

type ExceptGen = 
    static member Except () =
        gen {
            let! x = Arb.generate<int>
            let! failed = Arb.generate<bool>
            let! errorMsg = Arb.generate<string>

            return except {
                if failed
                then return! Except.throwError <| Error errorMsg
                else return x
            }
        }
        |> Arb.fromGen

let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<ExceptGen> ] }

[<Tests>]
let tests = 
    testList "Except Tests" [
        functorLaws fsCheckConfig defaultEquality exceptMonad
        applicativeLaws defaultEquality except
        monadLaws defaultEquality except

        let increment x = except {
            return x + 1
        }

        testProperty "Raising error tests Should should shortcircut computation" <| fun (x: int) (y: int) ->
            let fail x = Except.throwError <| Error $"%d{x}"
            let numberOfActions = ((abs x) % 1000) + 1
            let failAt = (abs y) % numberOfActions
            let shouldRun, shouldNotRun =
                List.replicate numberOfActions increment
                |> List.splitAt failAt

            let result = 
                shouldRun @ [fail] @ shouldNotRun
                |> List.fold (exceptMonad :> Monad<_>).Bind ((exceptMonad :> Monad<_>).Pure 0)
                |> Except.run

            result = Left (Error $"{failAt}")

        testProperty "When no error raised should return computation result" <| fun (x: int) -> 
            let numberOfActions = ((abs x) % 1000)
            let combinedAction =
                List.replicate numberOfActions increment
                |> List.fold (exceptMonad :> Monad<_>).Bind ((exceptMonad :> Monad<_>).Pure 0)

            Right numberOfActions = Except.run combinedAction
    ]
