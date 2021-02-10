module Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Generator, Seed)
import Svg exposing (Svg)
import Svg.Attributes as S
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT exposing (Paint(..), Transform(..))
import Util exposing (..)


{-|


# Todo

  - [x] implement TestMovingSphereSphere (RTC) and compute new ball velocity against single static ball.
      - [x] same with static balls list.

  - [x] bug: collision with multiple static balls, doesn't account for shortest collision.
    only the first found.

  - Bug: circle seg collision is causing penetration, maybe try seg-seg intersection.

  - Understand exactly how collision computes time `t` and document or return clarifying types.

  - bug: multiple collision i.e with edge and static ball at the same time;
    only respecting edge, and tunneling through static ball.

  - bug: ball edge tunnelling problem, since we are not doing continuous collision

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
    , floorBalls : List Ball
    , targets : List Target
    , seed : Seed
    }


type alias Target =
    { position : Vec
    , radius : Float
    , hp : Int
    }


decHP : Target -> Target
decHP target =
    { target | hp = target.hp - 1 }


hasHP : Target -> Bool
hasHP target =
    target.hp > 0


randomTarget : Generator Target
randomTarget =
    Random.map3 Target
        randomBallPosition
        (Random.int 30 30 |> Random.map toFloat)
        (Random.int 10 20)


type alias Ball =
    { position : Vec
    , angle : Float
    , speed : Float
    , hue : Float
    , radius : Float
    }


randomBall : Generator Ball
randomBall =
    Random.map5 Ball
        randomBallPosition
        --angle
        (Random.float 0 (turns 1))
        --speed
        (Random.float 10 15)
        --hue
        (Random.float 0 1)
        --radius
        (Random.int 10 30 |> Random.map toFloat)


randomBallPosition : Generator Vec
randomBallPosition =
    let
        gen e =
            Random.map2 (\offset -> vecMapR (add -offset))
                (Random.float 0 10)
                (randomVecOnSeg ( e.from, e.to ))
    in
    randomOneOf (List.map gen edges)
        |> Random.map (Maybe.withDefault vecZero)
        |> always (randomVecInRadii (sri |> vecScale 0.7))


ballVelocity : Ball -> Vec
ballVelocity ball =
    vecFromRTheta ball.speed ball.angle



--noinspection ElmUnusedSymbol


setBallPosition : Vec -> Ball -> Ball
setBallPosition v ball =
    { ball | angle = vecAngle v }


setBallPositionAndVelocity : Vec -> Vec -> Ball -> Ball
setBallPositionAndVelocity p v ball =
    { ball | position = p, angle = vecAngle v }


setBallVelocityAndUpdatePosition : Vec -> Ball -> Ball
setBallVelocityAndUpdatePosition rawVelocity ball =
    let
        angle =
            vecAngle rawVelocity

        velocity =
            vecFromRTheta ball.speed angle
    in
    { ball
        | position = vecAdd ball.position velocity
        , angle = angle
    }


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


isBottomEdge : Edge -> Bool
isBottomEdge edge =
    eqByAtLeast 0.01 (vecAngle edge.normal) (turns -0.25)


type Msg
    = OnTick Float


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        initialSeed =
            Random.initialSeed 0

        ( ( balls, targets ), seed ) =
            Random.step randomLevel initialSeed
    in
    ( { balls = balls
      , floorBalls = []
      , targets = targets
      , seed = seed
      }
    , Cmd.none
    )


randomLevel : Generator ( List Ball, List Target )
randomLevel =
    Random.pair
        (Random.list 10 randomBall)
        (Random.list 10 randomTarget)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnTick _ ->
            ( updateSim model
            , Cmd.none
            )


updateSim : Model -> Model
updateSim model =
    let
        ( targets, balls, floorBalls ) =
            model.balls
                |> List.foldl updateBall ( model.targets, [], [] )
    in
    { model
        | balls = balls
        , targets = targets
        , floorBalls = model.floorBalls ++ floorBalls
    }
        |> handleEmptyMovingBalls
        |> handleEmptyTargets


handleEmptyMovingBalls : Model -> Model
handleEmptyMovingBalls model =
    if model.balls == [] then
        { model
            | balls = model.floorBalls
            , floorBalls = []
        }

    else
        model


handleEmptyTargets : Model -> Model
handleEmptyTargets model =
    if model.targets == [] then
        let
            ( targets, seed ) =
                Random.step (Random.list 10 randomTarget) model.seed
        in
        { model | targets = targets, seed = seed }

    else
        model


type BallUpdate
    = BallMoved
    | BallHitTarget Target
    | BallHitBottomEdge


updateBall : Ball -> ( List Target, List Ball, List Ball ) -> ( List Target, List Ball, List Ball )
updateBall ball ( targets, acc, floored ) =
    let
        ( bu, newBall ) =
            updateBallHelp targets ball
    in
    case bu of
        BallMoved ->
            ( targets, newBall :: acc, floored )

        BallHitBottomEdge ->
            ( targets, acc, newBall :: floored )

        BallHitTarget target ->
            let
                nTargets =
                    targets
                        |> List.updateIf (eq target) decHP
                        |> List.filter hasHP
            in
            ( nTargets, newBall :: acc, floored )


type BallCollision
    = BallEdgeCollision Edge
    | BallTargetCollision Target


updateBallHelp : List Target -> Ball -> ( BallUpdate, Ball )
updateBallHelp targets ball =
    let
        velocity =
            ballVelocity ball
    in
    case detectBallCollision targets velocity ball of
        Nothing ->
            ( BallMoved, setBallVelocityAndUpdatePosition velocity ball )

        Just ( { t, normal }, bc ) ->
            let
                ballPositionAtT =
                    vecAdd ball.position (velocity |> vecScale t)

                newVelocity =
                    vecSub velocity (vecScale 2 (vecAlong normal velocity))

                newBall =
                    setBallPositionAndVelocity ballPositionAtT newVelocity ball
            in
            case bc of
                BallEdgeCollision e ->
                    ( if isBottomEdge e then
                        BallHitBottomEdge

                      else
                        BallMoved
                    , newBall
                    )

                BallTargetCollision target ->
                    ( BallHitTarget target, newBall )


detectBallCollision : List Target -> Vec -> Ball -> Maybe ( Collision, BallCollision )
detectBallCollision targets velocity ball =
    let
        mc =
            ( ( ball.position, ball.radius ), velocity )

        c1 =
            edges
                |> List.filterMap
                    (\e ->
                        detectMovingCircleAndSegCollision mc ( e.from, e.to )
                            |> Maybe.map (pairTo (BallEdgeCollision e))
                    )

        c2 =
            targets
                |> List.filterMap
                    (\o ->
                        detectMovingCircleAndCircleCollision mc ( o.position, o.radius )
                            |> Maybe.map (pairTo (BallTargetCollision o))
                    )
    in
    c1
        ++ c2
        |> List.minimumBy (Tuple.first >> .t)


type alias Collision =
    { t : Float, normal : Vec }


detectMovingCircleAndCircleCollision : ( Circle, Vec ) -> Circle -> Maybe Collision
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


detectMovingCircleAndSegCollision : ( Circle, Vec ) -> ( Vec, Vec ) -> Maybe Collision
detectMovingCircleAndSegCollision mc ( from, to ) =
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
            , viewBalls model.floorBalls
            , viewTargets model.targets
            , viewBalls model.balls
            , viewEdges
            ]
        ]


viewTargets : List Target -> Svg msg
viewTargets targets =
    let
        targetHue target =
            toFloat target.hp / 20

        targetColor target =
            Color.hsl (targetHue target) 0.7 0.6

        viewTarget target =
            Svg.g [ T.transform [ vecApply Translate target.position ] ]
                [ Svg.circle
                    [ Px.r target.radius
                    , T.fill (Paint (targetColor target))
                    ]
                    []
                , Svg.text_
                    [ S.fill "#fff"
                    , T.alignmentBaseline TT.AlignmentCentral
                    , T.textAnchor TT.AnchorMiddle
                    , T.transform [ scale (target.radius / 14) ]
                    ]
                    [ Svg.text (String.fromInt target.hp) ]
                ]
    in
    Svg.g [] (List.map viewTarget targets)


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
