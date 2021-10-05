module Kinda.Test.ApplicativeTests

open Expecto 

open Kinda.App
open Kinda.Applicative
open Kinda.Test.FunctorTests

module private Laws =
    let identity 
        (eq: Eq<'F>)
        (applicative: Applicative<'F>) 
        (fa: App<'F,'a>) =
        applicative.Apply (applicative.Pure id) fa 
        |> eq.AreEqual fa

    let homomorphism (eq: Eq<'F>) (applicative: Applicative<'F>) (f : 'a -> 'b) (x: 'a) = 
        let ff = applicative.Pure f
        let fx = applicative.Pure x

        applicative.Apply ff fx 
        |> eq.AreEqual (applicative.Pure (f x))

    let composition 
        (eq: Eq<'F>)
        (applicative: Applicative<'F>)
        (f: App<'F,'a -> 'b>)
        (g: App<'F,'b -> 'c>)
        (fa: App<'F,'a>) 
        =
        let ap a f = applicative.Apply f a
        let left = 
            applicative.Pure (>>)
            |> ap f
            |> ap g
            |> ap fa
        let right = 
            applicative.Apply f fa
            |> applicative.Apply g

        eq.AreEqual left right

    let interchange
        (eq: Eq<'F>)
        (applicative: Applicative<'F>)
        (f: App<'F, 'a -> 'b>)
        (x: 'a)
        =
        let left = applicative.Apply f <| applicative.Pure x
        let flipped = applicative.Pure ((|>) x)
        let right = applicative.Apply flipped f

        eq.AreEqual left right


let applicativeLaws (eq: Eq<'F>) (applicative: Applicative<'F>) = 
    testList "Applicative Laws" [
        testList "Identity" [
            let genericIdentity x = Laws.identity eq applicative (applicative.Pure x)

            testProperty "Int" genericIdentity<int>
            testProperty "String" genericIdentity<string>
            testProperty "List of tuples" genericIdentity<int*string list>
        ]

        testList "Homomorphism" [
            let genericHomomorphism x y = Laws.homomorphism eq applicative x y

            testProperty "One" <| fun (x: int) (y: int) -> Laws.homomorphism eq applicative ((+) x >> sprintf "%A") y
            testProperty "Two" genericHomomorphism<int,string>
            testProperty "Three" genericHomomorphism<int list,string option>
            testProperty "Four" <| genericHomomorphism Option.isSome
        ]

        testList "Composition" [
            let genericComposition f g x = Laws.composition eq applicative (applicative.Pure f) (applicative.Pure g) (applicative.Pure x)

            testProperty "One" <| genericComposition List.length (sprintf "%d")
            testProperty "Two" genericComposition<float option, Result<float, string>, string>
            testProperty "Three" genericComposition<int, string list, unit option>
        ]

        testList "Interchange" [
            let genericInterchange f x = Laws.interchange eq applicative (applicative.Pure f) x

            testProperty "One" <| genericInterchange id
            testProperty "Two" <| genericInterchange (sprintf "%A")
            testProperty "Three" <| genericInterchange List.length
        ]
    ]
