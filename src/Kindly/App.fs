module Kindly.App

type App<'hkt, 'a> = private App of obj

type App2<'f,'g,'a> = App<App<'f,'g>, 'a>

let unwrap (App x) = x
let create = App
