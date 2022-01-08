# Why

Small experimental library and takes a look how far we can take it.

# Goals and reasons

# HKT

## Emulation with [Lightweight higher-kinded polymorphism](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf)

```
'F<'a> -> App<'F,'a>
```

## Why does it matter

# Defining Functor/Applicative/Monad

Defining your own custom monad using Kinda requires some boilerplate to make things work.

## HKT boilerplare
First you have to define your generic type.

```fsharp
// We're defining custom type to make everything explicit
// We could just wrap F# option type
type private InnerMaybe<'a> = Nothing | Just of 'a
```

Then we're going to need HKT marker type. It's just used at type level, so it's useful to make it uninstantiable it by using [`Void`](../src/Kinda/Void.fs) type, since F# doesn't support empty types.

```fsharp
open Kinda.App

type MaybeH = private MH of Void
```

With that in place we can define our emulated HKT type synonym to make things a bit easier to read, like this:

```fsharp
type Maybe<'a> = App<MaybeH, 'a>
```

And also we're going to need a way to pack/unpack our generic type into/out of this HKT (to enrich it with marker type information):

```fsharp
module private MaybeH = 
    let inject (innerMaybe: InnerMaybe<'a>) : Maybe<'a> =
        create maybe

    let project (maybe: Maybe<'a>): InnerMaybe<'a> =
        unwrap maybe :?> _
```

## Implementing interfaces

```fsharp
type MaybeMonad () =
    interface Monad<MaybeH> with
        member _.Map (f : 'a -> 'b) (x: Maybe<'a>) : Maybe<'b> =
            // Get inner representation (InnerMaybe<'a>)
            MaybeH.project x
            |> (function 
                | Nothing -> Nothing
                | Just a  -> Just <| f a)
            // Inject it back into Maybe<'b> = App<MaybeH,'b> wrapper
            |> MaybeH.inject

        member _.Pure (x: 'a) : Maybe<'a> =
            //Just put it in Just (or any other monad you're implementing)
            Just x 
            //And again wrap it in App<MaybeH, 'a>
            |> MaybeH.inject

        member x.Apply (fab: Maybe<'a -> 'b>) (a: Maybe<'a>): Maybe<'b> =
            //Unwrap maybe implementation
            let maybeF = MaybeH.project fab

            match maybeF with
            //If you have no function, you can't apply it
            | Nothing -> Nothing
            //When you have a function, just map it over a
            | Just f -> (x :> Monad<_>).Map f a
            //And wrap it again
            |> MaybeH.inject

        member _.Bind (ma: Maybe<'a>) (f: 'a -> Maybe<'b>): Maybe<'b> =
            let maybeA = MaybeH.project ma
            match maybeA with
            | Nothing -> Nothing |> MaybeH.inject
            | Just a -> f a
```

## Useful helper functions

If you go down the road of hiding all implementation details, then it's useful to define module with some helper functions to make working with your monad easier.

```fsharp
module Maybe = 
    let just x = Just x |> MaybeH.inject
    let nothing<'a> = Nothing |> MaybeH.inject
    let default (val: 'a) (maybe: Maybe<'a>) : 'a = 
        MaybeH.project maybe
        |> (function 
            | Nothing -> val)
            | Just a -> a)
```

## Usage

```fsharp
// You can define monad instance anywhere...
let maybeMonad = MaybeMonad()
```


# Defining Monad Transformer
