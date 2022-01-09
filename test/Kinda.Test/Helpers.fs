module Kinda.Test.Helpers

open Expecto
open FsCheck

open Kinda.App

// Workaround to make type discovery work
type HktGen<'F> = 
    abstract Generate<'a> : unit -> Gen<App<'F, 'a>>

module HktGen = 
    let toGen (hktGen: HktGen<'F>)= Gen.constant hktGen
    let toArb (hktGen: HktGen<'F>)= toGen hktGen |> Arb.fromGen

type HktGenerators = 
    static member Generator<'F, 'a> () : Arbitrary<App<'F,'a>> = 
        gen {
            let! generator = Arb.generate<HktGen<'F>>
            return! generator.Generate<'a> ()
        } |> Arb.fromGen

module FsCheckConfig = 
    let addFunctorGen<'hktGenerator> (fsCheckConfig: FsCheckConfig): FsCheckConfig = 
        let newArbitrary = fsCheckConfig.arbitrary @ [ typeof<'hktGenerator>; typeof<HktGenerators>]
        { fsCheckConfig with arbitrary = newArbitrary }

    let withFunctorGen<'hktGenerator> = addFunctorGen<'hktGenerator> FsCheckConfig.defaultConfig