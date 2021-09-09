module Kindly.List

open Kindly.App
open Kindly.Monad

type List = 
    static member Inject (list: 'a list) : App<List, 'a> =
        create list
    static member Project (app: App<List, 'a>) : 'a list = 
        unwrap app :?> _

type ListMonad () =
    interface Monad<List> with
        member _.Map (f : 'a -> 'b) (x: App<List, 'a>) : App<List,'b> =
            List.Project x |> List.map f |> List.Inject

        member _.Pure (x : 'a) : App<List, 'a> = 
            List.Inject [x]

        member _.Apply (fab: App<List, 'a -> 'b>) (a: App<List,'a>) : App<List, 'b> = 
            let fabList = List.Project fab
            let aList = List.Project a
            let bList = List.collect (fun f -> List.map f aList) fabList
            List.Inject bList

        member _.Bind (ma: App<List, 'a>) (f : 'a -> App<List, 'b>) =
            let aList = List.Project ma
            let bList = List.collect (f >> List.Project) aList 
            List.Inject bList

    static member Instance = ListMonad () :> Monad<List>

