module Kinda.Test.ApplicativeTests

open Expecto 

open Kinda.App
open Kinda.Applicative
open Kinda.Test.FunctorTests

module private Laws =
    let identity<'F, 'a when 'a : equality>
        (eq: Eq<'F>)
        (applicative: Applicative<'F>) 
        (fa: App<'F,'a>) 
        =
        applicative.Apply (applicative.Pure id) fa 
        |> eq.AreEqual fa

    let homomorphism<'F, 'a, 'b when 'b : equality>
        (eq: Eq<'F>) 
        (applicative: Applicative<'F>) 
        (f : 'a -> 'b) (x: 'a) 
        = 
        let ff = applicative.Pure f
        let fx = applicative.Pure x

        applicative.Apply ff fx 
        |> eq.AreEqual (applicative.Pure (f x))

    let composition<'F, 'a, 'b, 'c when 'c : equality>
        (eq: Eq<'F>)
        (applicative: Applicative<'F>)
        (f: App<'F,'a -> 'b>)
        (g: App<'F,'b -> 'c>)
        (fa: App<'F,'a>) 
        =
        let ap a f = applicative.Apply f a
        let left = 
            applicative.Pure (<<)
            |> ap g
            |> ap f
            |> ap fa

        let right = 
            applicative.Apply f fa
            |> applicative.Apply g

        eq.AreEqual left right

    let interchange<'F, 'a, 'b when 'b : equality>
        (eq: Eq<'F>)
        (applicative: Applicative<'F>)
        (f: App<'F, 'a -> 'b>)
        (x: 'a)
        =
        let left = applicative.Apply f <| applicative.Pure x
        let flipped = applicative.Pure ((|>) x)
        let right = applicative.Apply flipped f

        eq.AreEqual left right


let applicativeLaws (fsCheckConfig: FsCheckConfig) (eq: Eq<'F>) (applicative: Applicative<'F>) = 
    let applicativeTest test = test eq applicative
    testList "Applicative Laws" [
        testList "Identity" [
            testPropertyWithConfig fsCheckConfig "Int" <| Laws.identity<'F, int> eq applicative
            testPropertyWithConfig fsCheckConfig "String" <| applicativeTest Laws.identity<'F,string>
            testPropertyWithConfig fsCheckConfig "List of tuples" <| applicativeTest Laws.identity<'F, int*string list>
        ]

        testList "Homomorphism" [
            testPropertyWithConfig fsCheckConfig "One" <| fun (x: int) (y: int) -> Laws.homomorphism eq applicative ((+) x >> sprintf "%A") y
            testPropertyWithConfig fsCheckConfig "Two" <| applicativeTest Laws.homomorphism<'F, int,string>
            testPropertyWithConfig fsCheckConfig "Three" <| applicativeTest Laws.homomorphism<'F, int list,string option>
            testPropertyWithConfig fsCheckConfig "Four" <| (applicativeTest Laws.homomorphism) Option.isSome
        ]

        testList "Composition" [
            testPropertyWithConfig fsCheckConfig "One" <| (applicativeTest Laws.composition) (applicative.Pure List.length) (applicative.Pure (sprintf "%d"))
            testPropertyWithConfig fsCheckConfig "Two" <| applicativeTest Laws.composition<'F, float option, Result<float, string>, string>
            testPropertyWithConfig fsCheckConfig "Three" <| applicativeTest Laws.composition<'F, int, string list, unit option>
        ]

        testList "Interchange" [
            let genericInterchange f x = Laws.interchange eq applicative (applicative.Pure f) x

            testPropertyWithConfig fsCheckConfig "One" <| genericInterchange id
            testPropertyWithConfig fsCheckConfig "Two" <| genericInterchange (sprintf "%A")
            testPropertyWithConfig fsCheckConfig "Three" <| genericInterchange List.length
            testPropertyWithConfig fsCheckConfig "Four" <| applicativeTest (Laws.interchange<'F, int, string>)
        ]
    ]
