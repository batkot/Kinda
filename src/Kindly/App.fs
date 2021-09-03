module Kindly.App

type App<'hkt, 'a> = App of obj

let unwrap (App x) = x
