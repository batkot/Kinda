module Kindly.Test.ApplicativeTests

open Expecto 

open Kindly.App
open Kindly.Applicative

module private Laws =
    let identity (applicative: Applicative<'F>) (fa: App<'F,'a>) =
        applicative.Apply (applicative.Pure id) fa = fa

    let homomorphism (applicative: Applicative<'F>) (f : 'a -> 'b) (x: 'a) = 
        let ff = applicative.Pure f
        let fx = applicative.Pure x
        applicative.Apply ff fx = applicative.Pure (f x)

    let composition 
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

        left = right

    let interchange
        (applicative: Applicative<'F>)
        (f: App<'F, 'a -> 'b>)
        (x: 'a)
        =
        let left = applicative.Apply f <| applicative.Pure x
        let flipped = applicative.Pure ((|>) x)
        let right = applicative.Apply flipped f

        left = right


let applicativeLaws (applicative: Applicative<'F>) = 
    testList "Applicative Laws" [
        testList "Identity" [
            let genericIdentity x = Laws.identity applicative (applicative.Pure x)

            testProperty "Int" genericIdentity<int>
            testProperty "String" genericIdentity<string>
            testProperty "List of tuples" genericIdentity<int*string list>
        ]

        testList "Homomorphism" [
            let genericHomomorphism x y = Laws.homomorphism applicative x y

            testProperty "One" <| fun (x: int) (y: int) -> Laws.homomorphism applicative ((+) x >> sprintf "%A") y
            testProperty "Two" genericHomomorphism<int,string>
            testProperty "Three" genericHomomorphism<int list,string option>
            testProperty "Four" <| Laws.homomorphism applicative Option.isSome
        ]

        testList "Composition" [
            let genericComposition f g x = Laws.composition applicative (applicative.Pure f) (applicative.Pure g) (applicative.Pure x)

            testProperty "One" <| genericComposition List.length (sprintf "%d")
            testProperty "Two" genericComposition<float option, Result<float, string>, string>
            testProperty "Three" genericComposition<int, string list, unit option>
        ]

        testList "Interchange" [
            let genericInterchange f x = Laws.interchange applicative (applicative.Pure f) x

            testProperty "One" <| genericInterchange id
            testProperty "Two" <| genericInterchange (sprintf "%A")
            testProperty "Three" <| genericInterchange List.length
        ]
    ]
