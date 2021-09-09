module Kindly.Test.MonadTests

open Expecto

open Kindly.App
open Kindly.Monad

module private Laws =

    let leftIdentity (monad: Monad<'M>) (f: 'a -> App<'M, 'b>) (x: 'a) =
        monad.Bind (monad.Pure x) f = f x

    let rightIdentity (monad: Monad<'M>) (m: App<'M, 'a>) =
        monad.Bind m monad.Pure = m
        
    let associativity 
        (monad: Monad<'M>) 
        (m: App<'M, 'a>) 
        (f: 'a -> App<'M,'b>) 
        (g: 'b -> App<'M,'c>)
        =
        let bind f m = monad.Bind m f
        let left = monad.Bind m (fun x -> monad.Bind (f x) g)
        let right = monad.Bind m f |> bind g
        left = right

let monadLaws (monad: Monad<'M>) = 
    testList "Monad Laws" [
        testList "Left identity" [
            let genericLeftId f x = Laws.leftIdentity monad (f >> monad.Pure) x

            testProperty "One" <| genericLeftId (sprintf "%A")
            testProperty "Two" <| genericLeftId (List.choose id >> List.length)
        ]

        testProperty "Right identity" <| fun x -> Laws.rightIdentity monad (monad.Pure x)

        testList "Associativity" [
            let genericAssociativity f g x = Laws.associativity monad (monad.Pure x) (f >> monad.Pure) (g >> monad.Pure)

            testProperty "One" <| fun (x: int) -> genericAssociativity id ((+)x)
            testProperty "Two" <| fun (x: int) -> genericAssociativity (+) ((|>) x)
        ]
    ]
