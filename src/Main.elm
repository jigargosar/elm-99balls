module Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import List.Extra as List
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as S
import Svg.Lazy exposing (lazy)
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types exposing (Paint(..), Transform(..))
import Util exposing (..)


{-|


# Todo

  - implement TestMovingSphereSphere (RTC) and compute new ball velocity against single static ball.
      - same with static balls list.
  - OR try continuous collision among balls
  - fix: ball edge tunnelling problem, since we are not doing continuous collision

-}
sw =
    600


sh =
    800


sri =
    vec sw sh |> vecScale 0.5


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flags =
    ()


type alias Model =
    { balls : List Ball
    , frames : Int
    , elapsed : Float
    }


type alias Ball =
    { position : Vec
    , angle : Float
    , speed : Float
    , hue : Float
    , radius : Float
    }


randomBalls : Generator (List Ball)
randomBalls =
    Random.list 10 randomBall


randomBall : Generator Ball
randomBall =
    Random.map5 Ball
        randomBallPosition
        --angle
        (Random.float 0 (turns 1))
        --speed
        (Random.float 1 2)
        --hue
        (Random.float 0 1)
        --radius
        (Random.int 5 50 |> Random.map toFloat)


randomBallPosition : Generator Vec
randomBallPosition =
    let
        gen e =
            Random.map2 (\offset -> vecMapR (add -offset))
                (Random.float 0 10)
                (randomVecOnLine e.from e.to)
    in
    randomOneOf (List.map gen edges)
        |> Random.map (Maybe.withDefault vecZero)
        |> always (randomVecInRadii sri)


type alias Edge =
    { from : Vec
    , to : Vec
    , midpoint : Vec
    , offset : Vec
    , normal : Vec
    }


edgeFromTo : Vec -> Vec -> Edge
edgeFromTo from to =
    let
        midpoint =
            vecMidpoint from to

        offset =
            vecFromTo midpoint to
    in
    { from = from
    , to = to
    , midpoint = midpoint
    , offset = offset
    , normal = vecUnitNormalFromTo from to
    }


edgeFromWithOffset : Vec -> Vec -> Edge
edgeFromWithOffset a b =
    edgeFromTo a (vecAdd a b)


edgePoints : Edge -> List Vec
edgePoints { from, to } =
    [ from, to ]


edgePoints2 : Edge -> List ( Float, Float )
edgePoints2 =
    edgePoints >> List.map vecToTuple


edges : List Edge
edges =
    let
        ( hw, hh ) =
            vecToTuple sri

        leftTop =
            vec -hw -hh

        rightTop =
            vec hw -hh

        rightBottom =
            vec hw hh

        leftBottom =
            vec -hw hh
    in
    [ edgeFromTo leftTop rightTop
    , edgeFromTo rightTop rightBottom
    , edgeFromTo rightBottom leftBottom
    , edgeFromTo leftBottom leftTop
    ]


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


type Msg
    = OnTick Float


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        initialSeed =
            Random.initialSeed 0

        ( balls, _ ) =
            Random.step randomBalls initialSeed
    in
    ( { balls = balls, frames = 0, elapsed = 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnTick _ ->
            ( updateSim model
            , Cmd.none
            )


updateSim : Model -> Model
updateSim model =
    { model | balls = List.map updateBall model.balls }


updateBall : Ball -> Ball
updateBall ball =
    let
        angle =
            computeNewBallVelocity ball |> vecAngle

        velocity =
            vecFromRTheta ball.speed angle
    in
    { ball
        | position = vecAdd ball.position velocity
        , angle = angle
    }


ballVelocity : Ball -> Vec
ballVelocity ball =
    vecFromRTheta ball.speed ball.angle


testMovingBalls : Ball -> Ball -> Maybe Float
testMovingBalls a b =
    testMovingSphereSphere ( a.position, a.radius, ballVelocity a )
        ( b.position, b.radius, ballVelocity b )


testMovingSphereSphere : ( Vec, Float, Vec ) -> ( Vec, Float, Vec ) -> Maybe Float
testMovingSphereSphere ( ac, ar, av ) ( bc, br, bv ) =
    let
        s =
            vecFromTo ac bc

        r =
            ar + br

        c =
            vecDotProduct s s - r ^ 2
    in
    if c < 0 then
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


computeNewBallVelocity : Ball -> Vec
computeNewBallVelocity ball =
    let
        velocity =
            vecFromRTheta ball.speed ball.angle
    in
    case List.find (ballEdgeCollision velocity ball) edges of
        Nothing ->
            velocity

        Just edge ->
            vecSub velocity (vecScale 2 (vecAlong edge.normal velocity))


ballEdgeCollision : Vec -> Ball -> Edge -> Bool
ballEdgeCollision velocity ball edge =
    let
        nextPosition =
            vecAdd ball.position velocity
    in
    (sqDistSegmentPoint ( edge.from, edge.to ) nextPosition <= ball.radius ^ 2)
        && (vecDotProduct velocity edge.normal < 0)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnTick
        ]


view : Model -> Html Msg
view model =
    Svg.svg
        [ T.viewBox (-sw / 2) (-sh / 2) sw sh
        , Px.width sw
        , Px.height sh
        , S.fill "none"
        , S.stroke "none"
        ]
        [ Svg.g [ T.transform [ scale 0.7 ] ]
            [ rect sw sh [] [ S.stroke "black" ]
            , lazy viewBalls model.balls
            , viewEdges
            ]
        ]


scale s =
    Scale s s


viewEdges : Svg Msg
viewEdges =
    Svg.g []
        [ Svg.g [] (List.map viewEdge edges)
        , Svg.g [] (List.map viewEdgeNormal edges)
        ]


viewEdge : Edge -> Svg Msg
viewEdge edge =
    Svg.polyline [ T.points (edgePoints2 edge), S.stroke "red", Px.strokeWidth 1 ] []


viewEdgeNormal : Edge -> Svg Msg
viewEdgeNormal edge =
    let
        normal =
            edgeFromWithOffset
                edge.midpoint
                (edge.normal |> vecScale (sw * 0.1))
    in
    Svg.polyline [ T.points (edgePoints2 normal), S.stroke "blue", Px.strokeWidth 5 ] []


viewBalls balls =
    Svg.g [] (List.map viewBall balls)


viewBallHelper x y nx ny radius hue =
    Svg.g [ T.stroke (Paint (Color.hsl hue 0.7 0.6)), T.transform [ Translate x y ] ]
        [ Svg.circle [ Px.r radius ] []
        , Svg.line [ Px.x2 nx, Px.y2 ny ] []
        ]


viewBall : Ball -> Svg Msg
viewBall ball =
    let
        ( x, y ) =
            vecToTuple ball.position
                |> roundFloat2

        ( nx, ny ) =
            ( ball.radius, ball.angle )
                |> fromPolar
                |> roundFloat2
    in
    viewBallHelper x y nx ny ball.radius ball.hue


roundFloat =
    round >> toFloat


roundFloat2 =
    mapEach roundFloat


rect w h xf aa =
    Svg.rect
        (Px.x (-w / 2)
            :: Px.y (-h / 2)
            :: Px.width w
            :: Px.height h
            :: T.transform xf
            :: aa
        )
        []
