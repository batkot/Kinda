module Kindly.Test.FunctorTests

open Expecto

open Kindly.App
open Kindly.Functor

let private functorIdentityLaw<'F, 'a> (functor: Functor<'F>) (f: App<'F,'a>) =
    id f = functor.Map id f

let private functorCompositionLaw<'F, 'a, 'b, 'c> 
    (functor: Functor<'F>)
    (f : 'a -> 'b)
    (g : 'b -> 'c)
    (fa: App<'F,'a>)
    =
    let functorComposed = functor.Map f >> functor.Map g
    functorComposed fa = functor.Map (f >> g) fa

let functorLaws fsCheckConfig (functor: Functor<'F>) = 
    testList "Functor Laws" [
        testList "Identity Law" [
            testPropertyWithConfig fsCheckConfig "Int" <| functorIdentityLaw<'F, int> functor
        ]
        testList "Composition Law" [
            let genericComposition (fa: App<'F, int>) (f: int -> 'b) (g: 'b -> 'c) = functorCompositionLaw functor f g fa

            testPropertyWithConfig fsCheckConfig "Int" <| fun (a: int) (b: int) (fa: App<'F, int>) -> functorCompositionLaw functor ((+) a) ((-) b) fa
            testPropertyWithConfig fsCheckConfig "?" genericComposition<string, int list>
        ]
    ]

