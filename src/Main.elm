module Main exposing (main)

import Browser
import Browser.Events
import Color exposing (..)
import Html exposing (Html)
import Html.Events as E
import Json.Decode as JD
import List.Extra as List
import Svg exposing (Svg)
import Svg.Attributes as S
import Tuple exposing (pair)
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

  - [x] generate targets in top row.

  - [x] Clarify current sim update intent.

  - [x] view input path

  - [x] fixed: using continuous collision. bug: multiple collision i.e with edge and static ball at the same time;
    only respecting edge, and tunneling through static ball.

  - [x] bug: ball edge tunnelling problem, since we are not doing continuous collision

  - model input
      - map mouse dx to aim angle.
          - try relative diff.

  - add nice `ting` sound on target ball collision

  - +1: Annoying. Bug: circle seg collision is causing penetration, maybe try seg-seg intersection.

  - Understand exactly how collision computes time `t` and document or return clarifying types.

  - Understand what happens, currently, when objects are already colliding,
    i.e. when objects already intersecting, what is effect on collision response.

  - Invalid states, that are representable.
      - ball outside screen edges / sim never ending.
      - total number of balls before and after simulation are different.

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
    { maybeEmitter : Maybe Emitter
    , balls : List Ball
    , floorBalls : List Ball
    , targets : List Target
    , state : State
    , pointerDown : Bool
    , pointer : Vec
    , prevPointer : Vec
    , frame : Float
    , seed : Seed
    }


animDur : Float
animDur =
    60 / 4


inputDur : Float
inputDur =
    200


type State
    = TargetsEntering Float
    | MockInput Float
    | WaitingForInput
    | DraggingPointer Float
    | Sim


type alias Emitter =
    { start : Float
    , next : Ball
    , rest : List Ball
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


moveTargetDown : Target -> Target
moveTargetDown target =
    { target | position = vecMapY (add (gc.cri.y * 2)) target.position }


randomTarget : ( Int, Int ) -> Generator Target
randomTarget gp =
    rnd1 (initTarget gp) (rndInt 1 maxHP)


initTarget : ( Int, Int ) -> Int -> Target
initTarget gp hp =
    Target (toWorld gp) gc.tr hp


canTargetsSafelyMoveDown : List Target -> Bool
canTargetsSafelyMoveDown targets =
    let
        maxGY =
            maximumBy (.position >> .y) targets
                |> Maybe.map (.position >> fromWorld >> snd)
                |> Maybe.withDefault -1
    in
    maxGY < (gc.h - 2)


type alias GridConf =
    { ri : Vec
    , w : Int
    , h : Int
    , cri : Vec
    , tr : Float
    , dx : Float
    , dy : Float
    , topRowPS : List ( Int, Int )
    }


gc : GridConf
gc =
    let
        ri =
            sri

        w =
            6

        h =
            8

        cri =
            vec (ri.x / w) (ri.y / h)

        tr =
            cri.x * 0.7

        ( dx, dy ) =
            ( cri.x - ri.x, cri.y - ri.y )

        topRowPS =
            List.range 0 (w - 1)
                |> List.concatMap (\x -> List.range 1 1 |> List.map (pair x))
    in
    { ri = ri
    , w = w
    , h = h
    , cri = cri
    , tr = tr
    , dx = dx
    , dy = dy
    , topRowPS = topRowPS
    }


toWorld : ( Int, Int ) -> Vec
toWorld ( x, y ) =
    let
        { cri, dy, dx } =
            gc
    in
    vec (toFloat x * cri.x * 2 + dx) (toFloat y * cri.y * 2 + dy)


fromWorld : Vec -> ( Int, Int )
fromWorld { x, y } =
    let
        { cri, dy, dx } =
            gc
    in
    ( (x - dx) / (cri.x * 2), (y - dy) / (cri.y * 2) )
        |> round2


randomTargets : Generator (List Target)
randomTargets =
    let
        randomTargetPositions =
            rnd2 List.drop (rndInt 1 3) (rndShuffle gc.topRowPS)
    in
    randomTargetPositions
        |> rndAndThen (List.map randomTarget >> rndCombine)


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
        (rndF (turns 0.6) (turns 0.9))
        --speed
        (rndF 10 15 |> always (rndConstant 15))
        --hue
        (rndF 0 1 |> always (rndConstant 0.15))
        --radius
        (rndInt 15 25 |> rnd1 toFloat |> always (rndConstant 20))


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
setBallPosition p ball =
    { ball | position = p }


setBallAngle : Float -> Ball -> Ball
setBallAngle a ball =
    { ball | angle = a }


setBallPositionAndVelocity : Vec -> Vec -> Ball -> Ball
setBallPositionAndVelocity p v ball =
    { ball | position = p, angle = vecAngle v }


setBallPositionAndAngle : Vec -> Float -> Ball -> Ball
setBallPositionAndAngle p angle ball =
    { ball | position = p, angle = angle }


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


edgeToSeg : Edge -> Seg
edgeToSeg { from, to } =
    ( from, to )


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
    | PointerDown Bool
    | PointerMoved Vec


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        initialSeed =
            seedFrom 4

        ( ( floorBalls, targets ), seed ) =
            rndStep ( randomLevel, initialSeed )
    in
    ( { maybeEmitter = Nothing
      , balls = []
      , floorBalls = floorBalls
      , targets = targets
      , pointerDown = False
      , pointer = vecZero
      , prevPointer = vecZero
      , state = TargetsEntering 0
      , frame = 0
      , seed = seed
      }
        |> addNewTargetRow
        |> addNewTargetRow
        |> addNewTargetRow
    , Cmd.none
    )


randomLevel : Generator ( List Ball, List Target )
randomLevel =
    rndPair
        (rndList 15 randomBall)
        randomTargets


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnTick _ ->
            ( updateOnTick model
                |> convergeFloorBalls
                |> incFrame
                |> cachePointer
            , Cmd.none
            )

        PointerDown isDown ->
            ( { model | pointerDown = isDown }, Cmd.none )

        PointerMoved pointer ->
            ( { model
                | pointer =
                    -- svg cords to world cords
                    vecAdd pointer (vecNegate gc.ri)
              }
            , Cmd.none
            )


cachePointer : Model -> Model
cachePointer model =
    { model | prevPointer = model.pointer }


updateOnTick : Model -> Model
updateOnTick model =
    case model.state of
        TargetsEntering start ->
            if
                (model.frame - start > animDur)
                    && areFloorBallsSettled model
            then
                { model
                    | state =
                        --MockInput model.frame
                        WaitingForInput
                }

            else
                model

        WaitingForInput ->
            if model.pointerDown then
                { model | state = DraggingPointer (turns -0.25) }

            else
                model

        DraggingPointer angle ->
            if not model.pointerDown then
                if shouldCancelInput model.pointer then
                    { model | state = Sim }
                        |> startSimAtAngle angle

                else
                    { model | state = WaitingForInput }

            else
                { model | state = DraggingPointer (inputAngle model.pointer) }

        MockInput start ->
            if model.frame - start > inputDur then
                { model | state = Sim }
                    |> startSim

            else
                model

        Sim ->
            -- check for turn over
            if (model.maybeEmitter == Nothing) && (model.balls == []) then
                -- check for game over
                if canTargetsSafelyMoveDown model.targets then
                    -- game over : for now re-simulate current turn.
                    { model | state = TargetsEntering model.frame }
                        |> addNewTargetRow

                else
                    { model | state = TargetsEntering model.frame }

            else
                model
                    |> moveBallsAndHandleCollision
                    |> emitBalls


shouldCancelInput : Vec -> Bool
shouldCancelInput { y } =
    y >= 0


inputAngle : Vec -> Float
inputAngle pointer =
    let
        dx =
            pointer.x / gc.ri.x

        angle =
            turns -0.25 + dx * turns 0.24

        _ =
            if clamp -1 1 dx == dx then
                ()

            else
                let
                    _ =
                        Debug.log "dx" dx
                in
                ()
    in
    angle


incFrame : Model -> Model
incFrame model =
    { model | frame = inc model.frame }


emitBalls : Model -> Model
emitBalls model =
    case model.maybeEmitter of
        Nothing ->
            model

        Just emitter ->
            if model.frame - emitter.start > 10 then
                { model
                    | balls = emitter.next :: model.balls
                    , maybeEmitter =
                        case emitter.rest of
                            [] ->
                                Nothing

                            n :: r ->
                                Just (Emitter model.frame n r)
                }

            else
                model


convergeFloorBalls : Model -> Model
convergeFloorBalls model =
    case model.floorBalls |> List.reverse of
        [] ->
            model

        f :: rest ->
            { model
                | floorBalls =
                    f
                        :: List.map (convergeBallTowards f.position) rest
                        |> List.reverse
            }


convergeBallTowards : Vec -> Ball -> Ball
convergeBallTowards to ball =
    let
        p =
            vecFromTo ball.position to
                |> vecScale 0.1
                |> vecAdd ball.position
    in
    setBallPosition p ball


moveBallsAndHandleCollision : Model -> Model
moveBallsAndHandleCollision model =
    let
        ( targets, floorBalls, balls ) =
            model.balls
                |> List.foldl updateBall ( model.targets, model.floorBalls, [] )
    in
    { model
        | targets = targets
        , floorBalls = floorBalls
        , balls = balls
    }


addNewTargetRow : Model -> Model
addNewTargetRow model =
    let
        ( targets, seed ) =
            rndStep ( randomTargets, model.seed )
    in
    { model
        | targets = targets ++ List.map moveTargetDown model.targets
        , seed = seed
    }


startSimAtAngle : Float -> Model -> Model
startSimAtAngle angle model =
    case model.floorBalls |> List.reverse of
        [] ->
            model

        f :: rest ->
            { model
                | floorBalls = []
                , maybeEmitter =
                    Just
                        (Emitter model.frame
                            (setBallAngle angle f)
                            (rest
                                |> List.map
                                    (setBallPositionAndAngle f.position angle)
                            )
                        )
            }


startSim : Model -> Model
startSim model =
    case model.floorBalls |> List.reverse of
        [] ->
            model

        f :: rest ->
            { model
                | floorBalls = []
                , maybeEmitter =
                    Just
                        (Emitter model.frame
                            f
                            (rest
                                |> List.map
                                    (setBallPositionAndVelocity f.position (ballVelocity f))
                            )
                        )
            }


areFloorBallsSettled : Model -> Bool
areFloorBallsSettled model =
    case model.floorBalls |> List.reverse of
        [] ->
            False

        f :: rest ->
            List.all (areBallsCloseEnough f) rest


areBallsCloseEnough : Ball -> Ball -> Bool
areBallsCloseEnough a b =
    vecLenSqFromTo a.position b.position
        |> eqByAtLeast 1 0


type BallUpdate
    = BallMoved
    | BallHitTarget Target
    | BallHitBottomEdge


updateBall : Ball -> ( List Target, List Ball, List Ball ) -> ( List Target, List Ball, List Ball )
updateBall ball ( targets, floored, acc ) =
    let
        ( bu, newBall ) =
            updateBallHelp targets ball
    in
    case bu of
        BallMoved ->
            ( targets, floored, newBall :: acc )

        BallHitBottomEdge ->
            ( targets, newBall :: floored, acc )

        BallHitTarget target ->
            let
                newTargets =
                    targets
                        |> mapWhenEq target decHP
                        |> keepWhen hasHP
            in
            ( newTargets, floored, newBall :: acc )


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
        , E.preventDefaultOn "pointerdown" (JD.succeed ( PointerDown True, True ))
        , E.preventDefaultOn "pointerup" (JD.succeed ( PointerDown False, True ))
        , E.preventDefaultOn "pointermove"
            (JD.map2 vec (JD.field "offsetX" JD.float) (JD.field "offsetY" JD.float)
                |> JD.map PointerMoved
                |> JD.map (pairTo True)
            )
        ]
        [ rect sri [ strokeP black ]
        , group [ transform [ scale 1 ] ]
            [ rect sri [ fillP black ]
            , viewFloorBalls model.floorBalls
            , case model.state of
                TargetsEntering start ->
                    viewTargets ((model.frame - start) / animDur |> clamp 0 1) model.targets

                _ ->
                    viewTargets 1 model.targets
            , viewBalls model.balls
            , viewEdges
            , case model.maybeEmitter of
                Just emitter ->
                    viewBalls [ emitter.next ]

                Nothing ->
                    viewNone
            , case model.state of
                MockInput start ->
                    group []
                        [ viewTravelPath model.frame (mockTravelPath start model)
                        , viewTravelPath model.frame (ballTravelPath model)
                        ]

                DraggingPointer angle ->
                    if shouldCancelInput model.pointer then
                        viewNone

                    else
                        group []
                            [ viewTravelPath model.frame
                                (ballTravelPathAtAngle angle model)
                            ]

                _ ->
                    viewNone
            , viewDebugPointer model.pointer |> always viewNone
            ]
        ]


viewFloorBalls floorBalls =
    case floorBalls of
        [] ->
            viewNone

        h :: t ->
            viewBalls (h :: (t |> reject (areBallsCloseEnough h)))


viewDebugPointer pointer =
    group []
        [ circle 10 [ fillH 0.4, transform [ translate pointer ] ]
        , polySeg ( vecZero, pointer ) [ strokeH 0.6 ]
        ]


viewTravelPath : Float -> List Vec -> Svg msg
viewTravelPath frame pts =
    let
        dash =
            10
    in
    polyline (List.reverse pts)
        [ strokeH 0.14
        , Px.strokeWidth 2
        , S.strokeDasharray (String.fromInt dash)
        , S.strokeDashoffset (round frame |> modBy (dash * 2) |> negate |> String.fromInt)
        ]


ballTravelPath : Model -> List Vec
ballTravelPath model =
    List.last model.floorBalls
        |> Maybe.map (ballTravelPathHelp model)
        |> Maybe.withDefault []


ballTravelPathAtAngle : Float -> Model -> List Vec
ballTravelPathAtAngle angle model =
    List.last model.floorBalls
        |> Maybe.map (setBallAngle angle >> ballTravelPathHelp model)
        |> Maybe.withDefault [ vecMidpoint bottomEdge.from bottomEdge.to ]


ballTravelPathHelp : Model -> Ball -> List Vec
ballTravelPathHelp model ball =
    ballTravelPathHelp2 model ball 0 [ ball.position ]


maxPathLen =
    gc.ri.y * 2


ballTravelPathHelp2 : Model -> Ball -> Float -> List Vec -> List Vec
ballTravelPathHelp2 model ball pathLen path =
    let
        ( bc, newBall ) =
            updateBallHelp model.targets ball

        newPathLenSq =
            vecLenFromTo ball.position newBall.position + pathLen
    in
    if newPathLenSq > maxPathLen || bc == BallHitBottomEdge then
        path

    else
        ballTravelPathHelp2 model newBall newPathLenSq (newBall.position :: path)


mockTravelPath : Float -> Model -> List Vec
mockTravelPath start model =
    let
        progress =
            (model.frame - start) / inputDur |> clamp 0 1

        angleOffset =
            0.05

        angle =
            turns (-0.5 + angleOffset) + turns (0.5 - angleOffset * 2) * progress
    in
    ballTravelPathAtAngle angle model


viewTargets : Float -> List Target -> Svg msg
viewTargets progress targets =
    let
        targetHue target =
            toFloat target.hp / maxHP

        dy =
            (1 - progress) * -(gc.cri.y * 2)

        viewTarget target =
            group [ transform [ translate (target.position |> vecMapY (add dy)) ] ]
                [ Svg.circle
                    [ Px.r target.radius
                    , fillH (targetHue target)
                    ]
                    []
                , words
                    (String.fromInt target.hp)
                    [ fillP black
                    , transform [ scale (target.radius / 14) ]
                    ]
                ]
    in
    group [] (List.map viewTarget targets)


viewEdges : Svg Msg
viewEdges =
    let
        do =
            group []
                [ group [] (List.map viewEdge edges)
                , group [] (List.map viewEdgeNormal edges)
                    |> always viewNone
                ]

        viewEdge : Edge -> Svg Msg
        viewEdge edge =
            polySeg (edgeToSeg edge)
                [ strokeP red, Px.strokeWidth 1 ]

        viewEdgeNormal : Edge -> Svg Msg
        viewEdgeNormal edge =
            polySeg (edgeDebugNormal edge)
                [ strokeP blue, Px.strokeWidth 5 ]

        edgeDebugNormal : Edge -> Seg
        edgeDebugNormal { from, to } =
            let
                start =
                    vecMidpoint from to

                velocity =
                    vecUnitNormalFromTo from to
                        |> vecScale (sw * 0.1)

                end =
                    vecAdd start velocity
            in
            ( start, end )
    in
    do


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
                    6
            in
            group
                [ strokeH hue
                , transform [ translate p ]
                , Px.strokeWidth strokeW
                ]
                [ Svg.circle [ Px.r (radius - strokeW / 2) ] []
                , Svg.line [ Px.x2 nx, Px.y2 ny ] []
                    |> always viewNone
                ]
    in
    do


polyline pts aa =
    Svg.polyline (points pts :: aa) []


polySeg ( a, b ) =
    polyline [ a, b ]


circle r aa =
    Svg.circle (Px.r r :: aa) []


rect ri =
    vecApply rectWH (vecScale 2 ri)


rectWH w h aa =
    Svg.rect
        (Px.x (-w / 2)
            :: Px.y (-h / 2)
            :: Px.width w
            :: Px.height h
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
    hsl hue 0.8 0.6


paintHue =
    fromHue >> Paint


strokeH =
    paintHue >> T.stroke


fillH =
    paintHue >> T.fill


group =
    Svg.g


viewNone =
    Svg.text ""
