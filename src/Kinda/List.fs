module Kinda.List

open Kinda.App
open Kinda.Monad
open Kinda.Void

type ListH = private LH of Void

type List<'a> = App<ListH, 'a>

module private ListH =
    let inject (list: 'a list) : List<'a> =
        create list
    let project (app: List<'a>) : 'a list = 
        unwrap app :?> _

module List =
    let fromList = ListH.inject

type ListMonad () =
    interface Monad<ListH> with
        member _.Map (f : 'a -> 'b) (x: List<'a>) : List<'b> =
            ListH.project x |> List.map f |> ListH.inject

        member _.Pure (x : 'a) : List<'a> = 
            ListH.inject [x]

        member _.Apply (fab: List<'a -> 'b>) (a: List<'a>) : List<'b> = 
            let fabList = ListH.project fab
            let aList = ListH.project a
            let bList = List.collect (fun f -> List.map f aList) fabList
            ListH.inject bList

        member _.Bind (ma: List<'a>) (f : 'a -> List<'b>) =
            let aList = ListH.project ma
            let bList = List.collect (f >> ListH.project) aList 
            ListH.inject bList

    static member Instance = ListMonad () :> Monad<ListH>

let list = monad <| ListMonad ()
