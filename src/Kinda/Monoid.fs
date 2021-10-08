module Kinda.Monoid

type Monoid<'m> =
    abstract Empty: 'm
    abstract Combine: 'm -> 'm -> 'm

module Monoid =
    let list = 
        { new Monoid<'a list> with 
            member _.Empty = []
            member _.Combine x y = x @ y
        }
