module Util exposing (..)

import Basics.Extra exposing (uncurry)


type alias Vec =
    { x : Float, y : Float }


vec : Float -> Float -> Vec
vec a b =
    Vec a b


vecFromTuple : ( Float, Float ) -> Vec
vecFromTuple ( a, b ) =
    vec a b


vecToTuple : Vec -> ( Float, Float )
vecToTuple =
    vecApply Tuple.pair


vecFromTo : Vec -> Vec -> Vec
vecFromTo a b =
    vecSub b a


vecFromRTheta : Float -> Float -> Vec
vecFromRTheta r theta =
    vecFromPolar ( r, theta )


vecFromPolar : ( Float, Float ) -> Vec
vecFromPolar =
    fromPolar >> vecFromTuple


vecToPolar : Vec -> ( Float, Float )
vecToPolar =
    vecToTuple >> toPolar


vecMap2 : (Float -> Float -> Float) -> Vec -> Vec -> Vec
vecMap2 fn a b =
    vec (fn a.x b.x) (fn a.y b.y)


vecMapEach : (Float -> Float) -> Vec -> Vec
vecMapEach fn { x, y } =
    vec (fn x) (fn y)


vecMapBoth : (Float -> Float) -> (Float -> Float) -> Vec -> Vec
vecMapBoth fx fy { x, y } =
    vec (fx x) (fy y)


vecMapY : (Float -> Float) -> Vec -> Vec
vecMapY fn =
    vecMapBoth identity fn


vecApply : (Float -> Float -> a) -> Vec -> a
vecApply fn { x, y } =
    fn x y


vecAdd : Vec -> Vec -> Vec
vecAdd =
    vecMap2 add


vecSub : Vec -> Vec -> Vec
vecSub =
    vecMap2 sub


vecScale : Float -> Vec -> Vec
vecScale s =
    vecMapEach (mul s)


vecNegate : Vec -> Vec
vecNegate =
    vecScale -1


add =
    (+)


sub =
    (-)


mul =
    (*)


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach fn =
    Tuple.mapBoth fn fn
