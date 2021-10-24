// Sketchbook
module Kinda.Test.Experimental

open Expecto

open Kinda.App
open Kinda.Monad
open Kinda.Identity
open Kinda.StateT
open Kinda.ReaderT

type StateMonadClass<'s,'M> =
    inherit Monad<'M>
    abstract member Get : App<'M,'s>
    abstract member Put : 's -> App<'M,unit>

type ReaderMonadClass<'r,'M> =
    inherit Monad<'M>
    abstract member Ask : App<'M, 'r>


module Generics = 
    type MyStackTH = ReaderTH<string,StateTH<int, IdentityH>>

    type MyStack () =
        let stack = ReaderTMonad(StateMonad())
        let monad = stack :> Monad<MyStackTH>

        member _.Run (env: string) (state: int) (x: ReaderT<string, StateTH<int, IdentityH>, 'a>) : (int * 'a)=
            x
            |> ReaderT.run env 
            |> State.run state

        interface Monad<MyStackTH> with
            member _.Map f x = monad.Map f x
            member _.Pure x = monad.Pure x
            member _.Apply fab a = monad.Apply fab a
            member _.Bind ma f = monad.Bind ma f

        interface StateMonadClass<int, MyStackTH> with
            member _.Get = MonadTrans.lift stack <| StateT.get (MonadTrans.innerMonad2 stack)
            member _.Put x = MonadTrans.lift stack <| StateT.put (MonadTrans.innerMonad2 stack) x

        interface ReaderMonadClass<string, MyStackTH> with
            member _.Ask = ReaderT.ask<_, string> <| MonadTrans.innerMonad stack

        member _.Hmm x : App<MyStackTH, 'a> = 
            Identity.fromA x
            |> MonadTrans.lift2 stack 


    let myStack = MyStack()

    let bar (m: StateTMonad<int,_, ReaderTMonad<string, _, _>>) : StateT<int, _, int>=
        monad m {
            let! counter = StateT.get <| MonadTrans.innerMonad m
            let! text = MonadTrans.lift m <| ReaderT.ask (MonadTrans.innerMonad2 m)
            return counter + String.length text
        }

    let hmm (m: ReaderTMonad<string, _, _>) x =
        let stack = monad m |> stateT
        stack {
            let! env = MonadTrans.lift stack <| ReaderT.ask (MonadTrans.innerMonad m)
            return x
        }

    let qwe: ReaderT<int, StateTH<int,ReaderTH<string,IdentityH>>, int> =
        let stack = reader |> stateT |> readerT 

        stack {
            let! x = ReaderT.ask <| MonadTrans.innerMonad stack

            let! y = 
                StateT.get (MonadTrans.innerMonad2 stack)
                |> MonadTrans.lift stack

            let! z = MonadTrans.lift2 stack <| Reader.ask

            do! StateT.put (MonadTrans.innerMonad2 stack) (x - y)
                |> MonadTrans.lift stack

            return x + y + (String.length z)
        }

    let yhm (m: MonadTransBuilder<_, ReaderTH<int, _>, ReaderTMonad<string, _, _>, _>) x = 
        let stack = stateT m 
        stack {
            let! env = MonadTrans.lift stack <| ReaderT.ask (MonadTrans.innerMonad m)
            let! state = StateT.get m
            return env + state + x
        }

    let addToState<'S, 'M when 'S :> StateMonadClass<int, 'M>> (m: 'S) x = 
            monad m {
                let! state = m.Get
                let result = state + x
                do! m.Put result
                return result
            } 

    let addFromReader<'S, 'M when 'S :> ReaderMonadClass<string, 'M>> 
        (m: 'S) (x: int) = 
            monad m {
                let! env = m.Ask
                return $"{env}-{x}"
            }

    let combined m x = 
        addToState m x 
        |> Monad.flipBind m (addFromReader m)


    [<Tests>]
    let tests = 
        testList "Generic param based type classess"
            // [ testCase "X" <| fun () -> 
            //     let (state, returned) = 
            //         monad myStack {
            //             let! result = addToState myStack 10
            //             return! addFromReader myStack result
            //         }
            //         |> myStack.Run "Test" 10

            //     Expect.equal state 20 "State should be maintained"
            //     Expect.equal returned "Test-20" "Reader should be pushed"
             
            [
            //   testCase "Y" <| fun () -> 
            //     let (_, (_, res))= 
            //         yhm (stateT reader) 10
            //         |> StateT.run 20
            //         |> StateT.run 10000
            //         |> Reader.run 30

            //     Expect.equal res 60 "?"

              testCase "Z" <| fun () -> 
                let (st, res) =
                    qwe 
                    |> ReaderT.run 15
                    |> StateT.run 10
                    |> Reader.run "ABCDE"

                Expect.equal res 30 "!"
                Expect.equal 5 st "!"
            ]

module Parameters =
    type Dependency<'a, 'b> = 'a -> 'b

    let justReader (reader: ReaderMonadClass<int, 'M>) = reader.Ask

    let parameterTest
        (m: Monad<'M>) 
        (state: StateMonadClass<int,'M>) = 
        monad m {
            let! st = state.Get
            do! state.Put <| st + 15
            return 0
        }

    let uncurriedTest
        ((m, state, reader): Monad<'M> * StateMonadClass<int, 'M> * ReaderMonadClass<int, 'M>) =
        monad m {
            let! env = justReader reader
            let! st = state.Get
            do! state.Put <| st + 15
            return 0
        }