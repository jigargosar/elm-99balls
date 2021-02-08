module Util exposing (..)

import Basics.Extra exposing (uncurry)
import Random exposing (Generator)


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


vecFromAngle : Float -> Vec
vecFromAngle angle =
    vecFromPolar ( 1, angle )


vecToPolar : Vec -> ( Float, Float )
vecToPolar =
    vecToTuple >> toPolar


vecMidpoint : Vec -> Vec -> Vec
vecMidpoint from to =
    vecFromTo from to
        |> vecScale 0.5
        |> vecAdd from


vecUnitNormalFromTo : Vec -> Vec -> Vec
vecUnitNormalFromTo from to =
    vecAngleFromTo from to
        |> add (turns 0.25)
        |> vecFromAngle


vecAngleFromTo : Vec -> Vec -> Float
vecAngleFromTo a b =
    vecFromTo a b |> vecAngle


vecAngle : Vec -> Float
vecAngle { x, y } =
    atan2 y x


vecNormalize : Vec -> Vec
vecNormalize v =
    vecFromAngle (vecAngle v)


vecScaleTo : Float -> Vec -> Vec
vecScaleTo n =
    vecNormalize >> vecScale n


vecDotProduct : Vec -> Vec -> Float
vecDotProduct a b =
    vecMap2 mul a b |> vecApply add


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


randomVec : Float -> Float -> Float -> Float -> Generator Vec
randomVec a b c d =
    Random.map2 vec (Random.float a b) (Random.float c d)


randomVecInRadii : Vec -> Generator Vec
randomVecInRadii ri =
    randomVec -ri.x ri.x -ri.y ri.y


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
