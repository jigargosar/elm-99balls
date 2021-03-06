module Util exposing (..)

import Basics.Extra exposing (atLeast)
import Color exposing (Color)
import Float.Extra
import Html exposing (Attribute, text)
import Html.Attributes exposing (style)
import Html.Events as E
import Json.Decode as JD exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Random.Extra as Random
import Random.Float
import Random.List
import TypedSvg.Attributes as T
import TypedSvg.Types exposing (..)


type alias Vec =
    { x : Float, y : Float }


vec : Float -> Float -> Vec
vec a b =
    Vec a b


vecZero : Vec
vecZero =
    vec 0 0


vecX x =
    vec x 0


vecY y =
    vec 0 y


vecFromTuple : ( Float, Float ) -> Vec
vecFromTuple ( a, b ) =
    vec a b


vecToTuple : Vec -> ( Float, Float )
vecToTuple =
    vecApply Tuple.pair


vecRound : Vec -> Vec
vecRound =
    vecMapEach roundFloat


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


angleABC : Vec -> Vec -> Vec -> Float
angleABC a b c =
    vecAngleFromTo b c - vecAngleFromTo b a


vecLenSqFromTo : Vec -> Vec -> Float
vecLenSqFromTo a b =
    vecFromTo a b |> vecLenSq


vecLenFromTo : Vec -> Vec -> Float
vecLenFromTo a b =
    vecFromTo a b |> vecLen


vecLen : Vec -> Float
vecLen =
    vecLenSq >> sqrt


vecLenSq : Vec -> Float
vecLenSq v =
    vecDotProduct v v


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


vecMapX : (Float -> Float) -> Vec -> Vec
vecMapX fn =
    vecMapBoth fn identity


vecDX : Float -> Vec -> Vec
vecDX dx =
    vecMapX (add dx)


vecDY : Float -> Vec -> Vec
vecDY dy =
    vecMapY (add dy)


vecApply : (Float -> Float -> a) -> Vec -> a
vecApply fn { x, y } =
    fn x y


vecAdd : Vec -> Vec -> Vec
vecAdd =
    vecMap2 add


vecMapLen : (Float -> Float) -> Vec -> Vec
vecMapLen fn =
    vecToPolar >> mapFst fn >> vecFromPolar


vecSub : Vec -> Vec -> Vec
vecSub =
    vecMap2 sub


vecScale : Float -> Vec -> Vec
vecScale s =
    vecMapEach (mul s)


vecUnscale : Float -> Vec -> Vec
vecUnscale s =
    vecScale (inv s)


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


rndVec : Float -> Float -> Float -> Float -> Generator Vec
rndVec a b c d =
    Random.map2 vec (Random.float a b) (Random.float c d)


rndUnitVec : Generator Vec
rndUnitVec =
    rndAngle |> rnd1 vecFromAngle


rndAngle : Generator Float
rndAngle =
    Random.float 0 1 |> rnd1 turns


rndVecInRadii : Vec -> Generator Vec
rndVecInRadii ri =
    rndVec -ri.x ri.x -ri.y ri.y


rndPtOnSeg : ( Vec, Vec ) -> Generator Vec
rndPtOnSeg ( a, b ) =
    Random.map2 vec
        (Random.float (min a.x b.x) (max a.x b.x))
        (Random.float (min a.y b.y) (max a.y b.y))


rndOneOf : List (Generator a) -> Generator (Maybe a)
rndOneOf xs =
    case xs of
        [] ->
            Random.constant Nothing

        h :: t ->
            Random.choices h t
                |> Random.map Just


isPointInRectRI : Vec -> Vec -> Bool
isPointInRectRI ri { x, y } =
    abs x < ri.x && abs y < ri.y


cornersFromRadii : Vec -> { leftTop : Vec, rightTop : Vec, rightBottom : Vec, leftBottom : Vec }
cornersFromRadii ri =
    let
        ( hw, hh ) =
            vecToTuple ri
    in
    { leftTop = vec -hw -hh
    , rightTop = vec hw -hh
    , rightBottom = vec hw hh
    , leftBottom = vec -hw hh
    }


type alias Seg =
    ( Vec, Vec )


boundingSegFromRadii : Vec -> { top : Seg, right : Seg, bottom : Seg, left : Seg }
boundingSegFromRadii ri =
    let
        { leftTop, rightTop, leftBottom, rightBottom } =
            cornersFromRadii ri
    in
    { top = ( leftTop, rightTop )
    , right = ( rightTop, rightBottom )
    , bottom = ( rightBottom, leftBottom )
    , left = ( leftBottom, leftTop )
    }


type alias Circle =
    ( Vec, Float )


type alias Collision =
    { t : Float, normal : Vec }


type alias MovingCircle =
    ( Circle, Vec )


detectMovingCircleAndCircleCollision : MovingCircle -> Circle -> Maybe Collision
detectMovingCircleAndCircleCollision mc c =
    testMovingSphereSphere mc ( c, vecZero )
        |> Maybe.filter (\t -> t >= 0 && t <= 1)
        |> Maybe.andThen
            (\t ->
                let
                    ( ( p1, _ ), velocity ) =
                        mc

                    ( p2, _ ) =
                        c

                    p1AtT =
                        vecAdd p1 (vecScale t velocity)

                    normal =
                        vecFromTo p2 p1AtT
                            |> vecNormalize
                in
                if vecDotProduct velocity normal < 0 then
                    Just (Collision t normal)

                else
                    Nothing
            )


detectMovingCircleAndSegCollision__Hack : MovingCircle -> ( Vec, Vec ) -> Maybe Collision
detectMovingCircleAndSegCollision__Hack mc ( from, to ) =
    testMovingSphereSphere mc ( ( from, 1 ), vecFromTo from to )
        |> Maybe.filter (\t -> t >= 0 && t <= 1)
        |> Maybe.andThen
            (\t ->
                let
                    ( _, velocity ) =
                        mc

                    normal =
                        vecUnitNormalFromTo from to
                in
                if vecDotProduct velocity normal < 0 then
                    Just (Collision t normal)

                else
                    Nothing
            )


detectMovingCircleAndSegCollision : MovingCircle -> Seg -> Maybe Collision
detectMovingCircleAndSegCollision mc s =
    let
        ( from, to ) =
            s

        normal =
            vecUnitNormalFromTo from to

        ( ( p, r ), v ) =
            mc
    in
    if vecDotProduct v normal < 0 then
        let
            shiftedSeg =
                s |> mapEach (vecAdd (vecScale r normal))

            circleSeg =
                ( p, vecAdd p v )
        in
        test2dSegSeg circleSeg shiftedSeg
            |> Maybe.andThen
                (\( t, _ ) ->
                    if t >= 0 && t <= 1 then
                        Just { t = t, normal = normal }

                    else
                        Nothing
                )

    else
        Nothing


{-| Book: Realtime Collision Detection
Page: 151
Section: 2D segment intersection
-}
test2dSegSeg__DoesntWork : Seg -> Seg -> Maybe ( Float, Vec )
test2dSegSeg__DoesntWork ( a, b ) ( c, d ) =
    let
        ( a1, a2 ) =
            ( signed2DTriArea a b d, signed2DTriArea a b c )
    in
    if a1 /= 0 && a2 /= 0 && a1 * a2 < 0 then
        let
            a3 =
                signed2DTriArea c d a

            a4 =
                a3 + a2 - a1
        in
        if a3 * a4 < 0 then
            let
                t =
                    a3 / (a3 - a4)
            in
            Just ( t, vecAdd a (vecScale t (vecSub b a)) )

        else
            Nothing

    else
        Nothing


signed2DTriArea : Vec -> Vec -> Vec -> Float
signed2DTriArea a b c =
    (a.x - c.y) * (b.y - c.y) - (a.y - c.y) * (b.x - c.x)


{-| wikipedia:
<https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line>
-}
test2dSegSeg : Seg -> Seg -> Maybe ( Float, Vec )
test2dSegSeg ( a, b ) ( c, d ) =
    let
        ( x1, y1 ) =
            vecToTuple a

        ( x2, y2 ) =
            vecToTuple b

        ( x3, y3 ) =
            vecToTuple c

        ( x4, y4 ) =
            vecToTuple d

        numerator =
            (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)

        denominator =
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    in
    if denominator /= 0 then
        let
            t =
                numerator / denominator

            p =
                vec (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))
        in
        Just ( t, p )

    else
        Nothing


sqDistSegmentPoint : Seg -> Vec -> Float
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


testMovingSphereSphere : ( Circle, Vec ) -> ( Circle, Vec ) -> Maybe Float
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


intersectRaySphere : ( Vec, Vec ) -> Circle -> Maybe Float
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


fullyElasticCollisionResponseVelocity : { a | normal : Vec } -> Vec -> Vec
fullyElasticCollisionResponseVelocity collision velocity =
    vecSub velocity (vecScale 2 (vecAlong collision.normal velocity))


type alias CollisionResponse =
    { position : Vec, velocity : Vec }


movingCircleCollisionResponse : MovingCircle -> Collision -> CollisionResponse
movingCircleCollisionResponse ( ( position, _ ), velocity ) { t, normal } =
    { position = vecAdd position (velocity |> vecScale t)
    , velocity = vecSub velocity (vecScale 2 (vecAlong normal velocity))
    }



-- Basics


add : number -> number -> number
add =
    (+)


sub : number -> number -> number
sub =
    (-)


mul : number -> number -> number
mul =
    (*)


inv : Float -> Float
inv n =
    1 / n


fdiv : Float -> Float -> Float
fdiv =
    (/)


idiv : Int -> Int -> Int
idiv =
    (//)


fmodBy : Float -> Float -> Float
fmodBy =
    Basics.Extra.fractionalModBy


eq : a -> a -> Bool
eq =
    (==)


cmdIf : Bool -> Cmd msg -> Cmd msg
cmdIf bool cmd =
    if bool then
        cmd

    else
        Cmd.none


inc : number -> number
inc =
    add 1


round2 : ( Float, Float ) -> ( Int, Int )
round2 =
    mapEach round


rangeF : Float -> Float -> Int -> List Float
rangeF start end steps =
    Float.Extra.range { start = start, end = end, steps = steps }


toFloat2 : ( Int, Int ) -> ( Float, Float )
toFloat2 =
    mapEach toFloat


roundFloat : Float -> Float
roundFloat =
    round >> toFloat


roundFloat2 : ( Float, Float ) -> ( Float, Float )
roundFloat2 =
    mapEach roundFloat


norm : Float -> Float -> Float -> Float
norm lo hi val =
    val - lo / hi - lo


toFrac : Float -> Float -> Float
toFrac duration elapsed =
    fmodBy duration elapsed / duration


zigZag : Float -> Float -> Float
zigZag duration elapsed =
    abs (2 * toFrac (duration * 2) elapsed - 1)


wave : Float -> Float -> Float
wave duration elapsed =
    (1 + cos (turns (toFrac (duration * 2) elapsed))) / 2


normToNegNorm : number -> number
normToNegNorm n =
    1 - n * 2


clampMO : number -> number -> number -> number
clampMO midA offA =
    if offA < 0 then
        clampMO midA -offA

    else
        let
            minA =
                midA - offA

            maxA =
                midA + offA
        in
        clamp minA maxA


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach fn =
    Tuple.mapBoth fn fn


pairTo : a -> b -> ( b, a )
pairTo b a =
    ( a, b )


fst : ( a, b ) -> a
fst =
    Tuple.first


snd : ( a, b ) -> b
snd =
    Tuple.second


mapFst : (a -> x) -> ( a, b ) -> ( x, b )
mapFst =
    Tuple.mapFirst


mapSnd : (b -> y) -> ( a, b ) -> ( a, y )
mapSnd =
    Tuple.mapSecond


eqByAtLeast : number -> number -> number -> Bool
eqByAtLeast tol a b =
    abs (a - b) < tol


lerp : Float -> Float -> Float -> Float
lerp =
    Float.Extra.interpolateFrom


when : (c -> Bool) -> (c -> c) -> c -> c
when pred fn x =
    if pred x then
        fn x

    else
        x


propEmpty : (c -> List a) -> c -> Bool
propEmpty fn =
    propEq fn []


propEq : (c -> b) -> b -> c -> Bool
propEq fn v x =
    fn x == v


atMost : comparable -> comparable -> comparable
atMost =
    Basics.Extra.atMost


atLeast : comparable -> comparable -> comparable
atLeast =
    Basics.Extra.atLeast


applyN : number -> (a -> a) -> a -> a
applyN n fn x =
    if n <= 0 then
        x

    else
        applyN (n - 1) fn (fn x)



-- List


mapWhen : (a -> Bool) -> (a -> a) -> List a -> List a
mapWhen =
    List.updateIf


mapWhenEq : a -> (a -> a) -> List a -> List a
mapWhenEq v =
    mapWhen (eq v)


keepWhen : (a -> Bool) -> List a -> List a
keepWhen =
    List.filter


reject : (a -> Bool) -> List a -> List a
reject =
    List.filterNot


remove : a -> List a -> List a
remove x =
    reject (eq x)


replace : b -> b -> List b -> List b
replace x y =
    List.setIf (eq x) y


minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy =
    List.minimumBy


maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy =
    List.maximumBy


maximum : List comparable -> Maybe comparable
maximum =
    List.maximum



-- Random


seedFrom : Int -> Random.Seed
seedFrom =
    Random.initialSeed


rndStep : ( Generator a, Random.Seed ) -> ( a, Random.Seed )
rndStep ( g, s ) =
    Random.step g s


rndLenList : Generator Int -> Generator a -> Generator (List a)
rndLenList lenGen gen =
    rndAndThen (\i -> rndList i gen) lenGen


rndPair : Generator a -> Generator b -> Generator ( a, b )
rndPair =
    Random.pair


rnd5 : (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
rnd5 =
    Random.map5


rnd4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
rnd4 =
    Random.map4


rnd2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
rnd2 =
    Random.map2


rnd3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
rnd3 =
    Random.map3


rnd1 : (a -> b) -> Generator a -> Generator b
rnd1 =
    Random.map


rndList : Int -> Generator a -> Generator (List a)
rndList =
    Random.list


shuffle : List a -> Generator (List a)
shuffle =
    Random.List.shuffle


rndConstant : a -> Generator a
rndConstant =
    Random.constant


rndAndThen : (a -> Generator b) -> Generator a -> Generator b
rndAndThen =
    Random.andThen


rndAndThen2 : (a -> b -> Generator c) -> Generator a -> Generator b -> Generator c
rndAndThen2 =
    Random.andThen2


rndShuffle : List a -> Generator (List a)
rndShuffle =
    Random.List.shuffle


rndInt : Int -> Int -> Generator Int
rndInt =
    Random.int


rndF : Float -> Float -> Generator Float
rndF =
    Random.float


rndCombine : List (Generator a) -> Generator (List a)
rndCombine =
    Random.combine


rndBellMO : Int -> Float -> Float -> Generator Float
rndBellMO n mid offset =
    rndBell n (mid - offset) (mid + offset)


rndBell : Int -> Float -> Float -> Generator Float
rndBell n lo hi =
    rndList n (rndF lo hi)
        |> rnd1 (List.sum >> (\total -> total / toFloat n))



-- SVG


strokeP : Color -> Attribute msg
strokeP =
    Paint >> T.stroke


fillP : Color -> Attribute msg
fillP =
    Paint >> T.fill


scale : Float -> Transform
scale s =
    Scale s s


transform : List Transform -> Attribute msg
transform =
    T.transform


translate : Vec -> Transform
translate =
    vecApply Translate


translateXY : Float -> Float -> Transform
translateXY =
    Translate


rotate deg =
    Rotate deg 0 0



-- Events


alwaysPreventDefaultOn : String -> Decoder a -> Html.Attribute a
alwaysPreventDefaultOn eventName decoder =
    E.preventDefaultOn eventName (JD.map (pairTo True) decoder)


offsetDecoder : Decoder Vec
offsetDecoder =
    JD.map2 vec (JD.field "offsetX" JD.float) (JD.field "offsetY" JD.float)


pageXYDecoder : Decoder Vec
pageXYDecoder =
    JD.map2 vec (JD.field "pageX" JD.float) (JD.field "pageY" JD.float)



-- HTML


maybeAttr : (a -> Html.Attribute msg) -> Maybe a -> Html.Attribute msg
maybeAttr attrFn mb =
    case mb of
        Nothing ->
            noAttr

        Just v ->
            attrFn v


maybeView : (a -> Html.Html msg) -> Maybe a -> Html.Html msg
maybeView viewFn mb =
    case mb of
        Nothing ->
            noView

        Just x ->
            viewFn x


noAttr : Html.Attribute msg
noAttr =
    style "" ""


noView : Html.Html msg
noView =
    text ""


hideView : b -> Html.Html msg
hideView =
    always noView


withoutCmd : a -> ( a, Cmd msg )
withoutCmd a =
    ( a, Cmd.none )
