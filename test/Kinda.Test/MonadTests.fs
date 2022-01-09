module Kinda.Test.MonadTests

open Expecto

open Kinda.App
open Kinda.Monad

open Kinda.Test.FunctorTests

module private Laws =
    let leftIdentity<'M, 'a, 'b when 'b : equality> (eq: Eq<'M>) (monad: Monad<'M>) (f: 'a -> App<'M, 'b>) (x: 'a) =
        monad.Bind (monad.Pure x) f 
        |> eq.AreEqual (f x)

    let rightIdentity<'M, 'a when 'a : equality> (eq: Eq<'M>) (monad: Monad<'M>) (m: App<'M, 'a>) =
        monad.Bind m monad.Pure
        |> eq.AreEqual m
        
    let associativity<'M, 'a, 'b, 'c when 'c : equality>
        (eq: Eq<'M>)
        (monad: Monad<'M>) 
        (m: App<'M, 'a>) 
        (f: 'a -> App<'M,'b>) 
        (g: 'b -> App<'M,'c>)
        =
        let bind f m = monad.Bind m f
        let left = monad.Bind m (fun x -> monad.Bind (f x) g)
        let right = monad.Bind m f |> bind g
        eq.AreEqual left right

let monadLaws (fsCheckConfig: FsCheckConfig) (eq: Eq<'M>) (monad: Monad<'M>) = 
    testList "Monad Laws" [
        testList "Left identity" [
            testPropertyWithConfig fsCheckConfig "One" <| Laws.leftIdentity<'M, int, string> eq monad
            testPropertyWithConfig fsCheckConfig "Two" <| Laws.leftIdentity<'M, string list, int option> eq monad
        ]

        testPropertyWithConfig fsCheckConfig "Right identity" <| Laws.rightIdentity<'M, int> eq monad

        testList "Associativity" [
            testPropertyWithConfig fsCheckConfig "One" <| Laws.associativity<'M, int, Result<string,int>, bool list> eq monad 
            testPropertyWithConfig fsCheckConfig "Two" <| Laws.associativity<'M, int list, bool option, string> eq monad 
        ]
    ]
