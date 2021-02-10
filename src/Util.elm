module Util exposing (..)

import Basics.Extra exposing (atLeast, uncurry)
import Random exposing (Generator)
import Random.Extra as Random


type alias Vec =
    { x : Float, y : Float }


vec : Float -> Float -> Vec
vec a b =
    Vec a b


vecZero : Vec
vecZero =
    vec 0 0


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


vecMapR : (Float -> Float) -> Vec -> Vec
vecMapR fn =
    vecToPolar >> Tuple.mapFirst fn >> vecFromPolar


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


vecAlong : Vec -> Vec -> Vec
vecAlong directionVec v =
    let
        n =
            vecNormalize directionVec

        mag =
            vecDotProduct n v
    in
    vecScale mag n


randomVec : Float -> Float -> Float -> Float -> Generator Vec
randomVec a b c d =
    Random.map2 vec (Random.float a b) (Random.float c d)


randomVecInRadii : Vec -> Generator Vec
randomVecInRadii ri =
    randomVec -ri.x ri.x -ri.y ri.y


randomVecOnSeg : ( Vec, Vec ) -> Generator Vec
randomVecOnSeg ( a, b ) =
    Random.map2 vec
        (Random.float (min a.x b.x) (max a.x b.x))
        (Random.float (min a.y b.y) (max a.y b.y))


randomOneOf : List (Generator a) -> Generator (Maybe a)
randomOneOf xs =
    case xs of
        [] ->
            Random.constant Nothing

        h :: t ->
            Random.choices h t
                |> Random.map Just


type alias Circle =
    ( Vec, Float )


sqDistSegmentPoint : ( Vec, Vec ) -> Vec -> Float
sqDistSegmentPoint ( a, b ) c =
    -- Book: realtime collision detection
    -- Page 130
    let
        ( ab, ac, bc ) =
            ( vecFromTo a b, vecFromTo a c, vecFromTo b c )

        ( e, f ) =
            ( vecDotProduct ac ab, vecDotProduct ab ab )
    in
    if e <= 0 then
        vecDotProduct ab ab

    else if e >= f then
        vecDotProduct bc bc

    else
        vecDotProduct ac ac - e * e / f


testMovingSphereSphere : ( ( Vec, Float ), Vec ) -> ( ( Vec, Float ), Vec ) -> Maybe Float
testMovingSphereSphere ( ( ac, ar ), av ) ( ( bc, br ), bv ) =
    -- Book: realtime collision detection
    -- Page 223
    let
        s =
            vecFromTo ac bc

        r =
            ar + br

        c =
            vecDotProduct s s - r ^ 2
    in
    if c < 0 then
        -- Exit early if already colliding
        Just 0
        --Nothing
        -- Mod: Ensuring that circles are approaching one another if already colliding
        --if vecDotProduct av bv < 0 then
        --    Just 0
        --
        --else
        --    Nothing

    else
        let
            v =
                vecFromTo av bv

            a =
                vecDotProduct v v
        in
        if a < 0.001 then
            Nothing

        else
            let
                b =
                    vecDotProduct v s
            in
            if b >= 0 then
                Nothing

            else
                let
                    d =
                        b ^ 2 - a * c
                in
                if d < 0 then
                    Nothing

                else
                    Just ((-b - sqrt d) / a)


intersectRaySphere : ( Vec, Vec ) -> ( Vec, Float ) -> Maybe Float
intersectRaySphere ( p, d ) ( sc, sr ) =
    let
        m =
            vecSub p sc

        b =
            vecDotProduct m d

        c =
            vecDotProduct m m - sr ^ 2
    in
    if c > 0 && b > 0 then
        Nothing

    else
        let
            discriminant =
                b ^ 2 - c
        in
        if discriminant < 0 then
            Nothing

        else
            (-b - sqrt discriminant)
                |> atLeast 0
                |> Just


add =
    (+)


sub =
    (-)


mul =
    (*)


eq =
    (==)


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach fn =
    Tuple.mapBoth fn fn


pairTo b a =
    ( a, b )


eqByAtLeast tol a b =
    abs (a - b) < tol


when pred fn x =
    if pred x then
        fn x

    else
        x


propEmpty fn =
    propEq fn []


propEq fn v x =
    fn x == v
