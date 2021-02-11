module Main exposing (main)

import Browser
import Browser.Events
import Color exposing (..)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as S
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types exposing (..)
import Util exposing (..)


{-|


# Todo

  - [x] implement TestMovingSphereSphere (RTC) and compute new ball velocity against single static ball.
      - [x] same with static balls list.

  - [x] bug: collision with multiple static balls, doesn't account for shortest collision.
    only the first found.

  - generate targets in top row.

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


maxHP =
    20


randomTargets : Generator (List Target)
randomTargets =
    let
        gri =
            sri

        gw =
            6

        gh =
            8

        cri =
            vec (gri.x / gw) (gri.y / gh)

        targetRadius =
            cri.x * 0.7

        ( dx, dy ) =
            ( cri.x - gri.x, cri.y - gri.y )

        gps =
            List.range 0 (gw - 1)
                |> List.concatMap
                    (toFloat
                        >> (\x ->
                                List.range 1 4
                                    |> List.map
                                        (toFloat
                                            >> (\y ->
                                                    vec (x * cri.x * 2 + dx) (y * cri.y * 2 + dy)
                                               )
                                        )
                           )
                    )

        randomTargetPositions =
            gps
                |> rndShuffle
                |> rnd2 List.drop (rndConstant 10)
    in
    randomTargetPositions
        |> rndAndThen
            (\ps ->
                ps
                    |> List.map
                        (\p ->
                            rnd1 (Target p targetRadius)
                                (rndI 1 (maxHP // 3))
                        )
                    |> rndCombine
            )



--randomTarget : Generator Target
--randomTarget =
--    Random.map3 Target
--        (randomVecInRadii (sri |> vecScale 0.7))
--        --radius
--        (Random.int 25 30 |> Random.map toFloat)
--        --hp
--        (Random.int (maxHP // 2) maxHP)


type alias Ball =
    { position : Vec
    , angle : Float
    , speed : Float
    , hue : Float
    , radius : Float
    }


randomBall : Generator Ball
randomBall =
    rnd5 Ball
        randomBallPositionAtBottom
        --angle
        (rndF 0 (turns 1))
        --speed
        (rndF 10 15)
        --hue
        (rndF 0 1)
        --radius
        (rndI 15 25 |> rnd1 toFloat)


randomBallPositionAtBottom : Generator Vec
randomBallPositionAtBottom =
    --Random.map (vecScale 1)
    rnd2 (\offset -> vecMapR (add -offset))
        (rndF 0 10)
        (randomPtOnSeg screenSeg.bottom)


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
    }


edgeFromSeg : Seg -> Edge
edgeFromSeg ( a, b ) =
    edgeFromTo a b


edgeFromTo : Vec -> Vec -> Edge
edgeFromTo from to =
    { from = from
    , to = to
    }


edgePoints : Edge -> List Vec
edgePoints { from, to } =
    [ from, to ]


screenSeg =
    boundingSegFromRadii sri


edges : List Edge
edges =
    let
        { top, right, bottom, left } =
            screenSeg
    in
    [ top, right, bottom, left ]
        |> List.map edgeFromSeg


bottomEdge =
    edgeFromSeg screenSeg.bottom


isBottomEdge : Edge -> Bool
isBottomEdge edge =
    edge == bottomEdge


type Msg
    = OnTick Float


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        initialSeed =
            seedFrom 0

        ( ( balls, targets ), seed ) =
            rndStep ( randomLevel, initialSeed )
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
    rndPair
        (rndList 10 randomBall)
        randomTargets


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnTick _ ->
            ( updateSim model
            , Cmd.none
            )


updateSim : Model -> Model
updateSim model =
    model
        |> moveSim
        |> handleEmptyMovingBalls
        |> handleEmptyTargets


moveSim : Model -> Model
moveSim model =
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


handleEmptyMovingBalls : Model -> Model
handleEmptyMovingBalls model =
    if model.balls == [] && model.targets /= [] then
        { model
            | balls = model.floorBalls
            , floorBalls = []
        }

    else
        model


handleEmptyTargets : Model -> Model
handleEmptyTargets model =
    if model.targets == [] && model.balls == [] then
        let
            ( targets, seed ) =
                rndStep ( randomTargets, model.seed )
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
                newTargets =
                    targets
                        |> mapWhenEq target decHP
                        |> keepWhen hasHP
            in
            ( newTargets, newBall :: acc, floored )


type BallCollision
    = BallEdgeCollision Edge
    | BallTargetCollision Target


updateBallHelp : List Target -> Ball -> ( BallUpdate, Ball )
updateBallHelp targets ball =
    let
        velocity =
            ballVelocity ball
                |> vecMapY (add 0.01)
    in
    case detectBallCollision targets velocity ball of
        Nothing ->
            ( BallMoved, setBallVelocityAndUpdatePosition velocity ball )

        Just ( response, ballCollision ) ->
            ( case ballCollision of
                BallEdgeCollision e ->
                    if isBottomEdge e then
                        BallHitBottomEdge

                    else
                        BallMoved

                BallTargetCollision target ->
                    BallHitTarget target
            , setBallPositionAndVelocity response.position response.velocity ball
            )


type alias CollisionResponse =
    { position : Vec, velocity : Vec }


detectBallCollision : List Target -> Vec -> Ball -> Maybe ( CollisionResponse, BallCollision )
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
        |> minimumBy (fst >> .t)
        |> Maybe.map (mapFst (movingCircleCollisionResponse mc))


movingCircleCollisionResponse : MovingCircle -> Collision -> CollisionResponse
movingCircleCollisionResponse ( ( position, _ ), velocity ) { t, normal } =
    { position = vecAdd position (velocity |> vecScale t)
    , velocity = vecSub velocity (vecScale 2 (vecAlong normal velocity))
    }


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
        [ group [ transform [ scale 0.7 ] ]
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
            toFloat target.hp / maxHP

        viewTarget target =
            group [ transform [ translate target.position ] ]
                [ Svg.circle
                    [ Px.r target.radius
                    , fillH (targetHue target)
                    ]
                    []
                , words
                    (String.fromInt target.hp)
                    [ fillP white
                    , transform [ scale (target.radius / 14) ]
                    ]
                ]
    in
    group [] (List.map viewTarget targets)


viewEdges : Svg Msg
viewEdges =
    group []
        [ group [] (List.map viewEdge edges)
        , group [] (List.map viewEdgeNormal edges)
        ]


viewEdge : Edge -> Svg Msg
viewEdge edge =
    Svg.polyline [ points (edgePoints edge), strokeP red, Px.strokeWidth 1 ] []


viewEdgeNormal : Edge -> Svg Msg
viewEdgeNormal edge =
    let
        from =
            vecMidpoint edge.from edge.to

        velocity =
            vecUnitNormalFromTo edge.from edge.to
                |> vecScale (sw * 0.1)

        to =
            vecAdd from velocity
    in
    Svg.polyline [ points [ from, to ], strokeP blue, Px.strokeWidth 5 ] []


viewBalls : List Ball -> Svg Msg
viewBalls =
    let
        do balls =
            group [] (List.map viewBall balls)

        viewBall : Ball -> Svg Msg
        viewBall ball =
            let
                p =
                    vecRound ball.position

                ( nx, ny ) =
                    ( ball.radius, ball.angle )
                        |> fromPolar
                        |> roundFloat2
            in
            let
                { radius, hue } =
                    ball
            in
            let
                strokeW =
                    4
            in
            group
                [ strokeH hue
                , transform [ translate p ]
                , Px.strokeWidth strokeW
                ]
                [ Svg.circle [ Px.r (radius - strokeW / 2) ] []
                , Svg.line [ Px.x2 nx, Px.y2 ny ] []
                ]
    in
    do


rect w h xf aa =
    Svg.rect
        (Px.x (-w / 2)
            :: Px.y (-h / 2)
            :: Px.width w
            :: Px.height h
            :: transform xf
            :: aa
        )
        []


words txt aa =
    Svg.text_
        (T.alignmentBaseline AlignmentCentral
            :: T.textAnchor AnchorMiddle
            :: aa
        )
        [ Svg.text txt ]


points =
    List.map vecToTuple >> T.points


fromHue hue =
    hsl hue 0.9 0.45


paintHue =
    fromHue >> Paint


strokeH =
    paintHue >> T.stroke


fillH =
    paintHue >> T.fill


group =
    Svg.g
