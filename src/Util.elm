module Util exposing (..)


type alias Vec =
    { x : Float, y : Float }


vec : Float -> Float -> Vec
vec a b =
    Vec a b


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach fn =
    Tuple.mapBoth fn fn
