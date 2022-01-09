module Kinda.IO

open Kinda.App
open Kinda.Void
open Kinda.ReaderT
open Kinda.Monad

type private InnerIO<'a> = MkIO of Reader<unit, 'a>
let private getIOAction (MkIO action) = action

type IOH = private IOH of Void

type IO<'a> = App<IOH, 'a>

module private IOH = 
    let inject (io: InnerIO<'a>) : IO<'a> = 
        create io

    let project (app: IO<'a>) : InnerIO<'a> =
        unwrap app :?> _

module IO = 
    let run (io: IO<'a>) : 'a =
        IOH.project io |> getIOAction |> Reader.run ()

    let wrapIO (ioAction: unit -> 'a) = 
        Reader.fromFunction ioAction
        |> MkIO
        |> IOH.inject

    let wrapIO1 (ioAction: 'a -> 'b) x =
        wrapIO <| fun () -> ioAction x

    let wrapIO2 (ioAction: 'a -> 'b -> 'c) x =
        wrapIO1 <| ioAction x

    let wrapIO3 (ioAction: 'a -> 'b -> 'c -> 'd) x =
        wrapIO2 <| ioAction x


type IOMonad () =
    interface Monad<IOH> with
        member _.Map (f: 'a -> 'b) (ioA: IO<'a>) : IO<'b> = 
            reader {
                let! a = IOH.project ioA |> getIOAction
                return f a
            } |> MkIO |> IOH.inject

        member _.Pure (x: 'a) : IO<'a> =
            IO.wrapIO <| fun () -> x

        member _.Apply (ioFab: IO<'a -> 'b>) (ioA: IO<'a>): IO<'b> =
            reader {
                let! fab = IOH.project ioFab |> getIOAction
                let! a = IOH.project ioA |> getIOAction
                return fab a
            } |> MkIO |> IOH.inject

        member _.Bind (ioA: IO<'a>) (f: 'a -> IO<'b>) : IO<'b> = 
            reader {
                let! a = IOH.project ioA |> getIOAction
                return! f a |> IOH.project |> getIOAction
            } |> MkIO |> IOH.inject

    static member Instance = IOMonad()

let io = monad <| IOMonad()