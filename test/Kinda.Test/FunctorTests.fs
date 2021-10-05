module Kinda.Test.FunctorTests

open Expecto

open Kinda.App
open Kinda.Functor

type Eq<'F> = 
    abstract AreEqual<'a when 'a : equality> : App<'F,'a> -> App<'F,'a> -> bool

let defaultEquality<'F> = 
    { new Eq<'F> with 
        member _.AreEqual x y = x = y
    }

let private functorIdentityLaw<'F, 'a when 'a : equality> (eq: Eq<'F>) (functor: Functor<'F>) (f: App<'F,'a>) =
    eq.AreEqual f <| functor.Map id f

let private functorCompositionLaw<'F, 'a, 'b, 'c when 'c : equality> 
    (eq: Eq<'F>)
    (functor: Functor<'F>)
    (f : 'a -> 'b)
    (g : 'b -> 'c)
    (fa: App<'F,'a>)
    =
    let functorComposed = functor.Map f >> functor.Map g
    functorComposed fa 
    |> eq.AreEqual (functor.Map (f >> g) fa)


let functorLaws fsCheckConfig (eq: Eq<'F>) (functor: Functor<'F>) = 
    testList "Functor Laws" [
        testList "Identity Law" [
            testPropertyWithConfig fsCheckConfig "Int" <| functorIdentityLaw<'F, int> eq functor
        ]
        testList "Composition Law" [
            let genericComposition (fa: App<'F, int>) (f: int -> 'b) (g: 'b -> 'c) = functorCompositionLaw eq functor f g fa

            testPropertyWithConfig fsCheckConfig "Int" <| fun (a: int) (b: int) (fa: App<'F, int>) -> functorCompositionLaw eq functor ((+) a) ((-) b) fa
            testPropertyWithConfig fsCheckConfig "?" genericComposition<string, int list>
        ]
    ]

