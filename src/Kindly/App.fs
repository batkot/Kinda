module Kindly.App

type App<'hkt, 'a> = private App of obj

let unwrap (App x) = x
let create = App
