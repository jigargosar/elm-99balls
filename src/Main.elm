module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Color exposing (..)
import Html exposing (Attribute, Html, div, node, text)
import Html.Attributes as A exposing (style)
import Html.Events as E exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Random.Extra as Random
import Svg exposing (Svg)
import Svg.Attributes as S
import Task
import Tuple exposing (pair)
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types exposing (..)
import Util exposing (..)


{-|


# Todo

  - [x] Bug: angle always showing at center
      - for v1 on mobile, check other cases.

  - [x] allow multiple input handling mode for quick switch during testing
      - for now we are using hardcoded constant for switching

  - [x] goodies: extra ball

  - [x] game over

  - turn relative ball count. don't spend too much time. also check the 99balls game.

  - test input handling on phone/touch device.
      - Concern: angle flickering on touch release


# Later

  - add nice `ting` sound on target ball collision

  - Understand exactly how collision computes time `t` and document or return clarifying types.

  - Understand what happens, currently, when objects are already colliding,
    i.e. when objects already intersecting, what is effect on collision response.

  - Invalid states, that are representable.
      - +1 ball outside screen edges / sim never ending.
          - Bug: ball outside world, invariant failed during mobile testing InputV1
      - total number of balls before and after simulation are different.

  - -1 Concern: Input handling is much better, but there is room for improvement.
      - carpel tunnel pain: try tap rather than drag, perhaps only for mouse

-}
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
    { turn : Int
    , ballCount : Int
    , targets : List Target
    , state : State
    , pointerDown : Bool
    , prevPointerDown : Bool
    , pointer : Vec
    , prevPointer : Vec
    , frame : Float
    , vri : Vec
    , seed : Seed
    }


transitionDuration : Float
transitionDuration =
    60 / 4


transitionProgress : Float -> Float -> Float
transitionProgress start now =
    (now - start) / transitionDuration |> clamp 0 1


transitionDone : Float -> Float -> Bool
transitionDone start now =
    transitionProgress start now >= 1


type State
    = Running RunState
    | GameLost Float


type RunState
    = TargetsEntering { start : Float, ballPosition : Vec }
    | WaitingForInput { ballPosition : Vec }
    | DraggingPointer { dragStartAt : Vec, ballPosition : Vec }
    | SimTag Sim


type alias Sim =
    { mbEmitter : Maybe Emitter, balls : List Ball, floored : List Ball }


type alias Emitter =
    { start : Float
    , next : Ball
    , rest : List Ball
    }


type TargetKind
    = SolidTarget Int
    | ExtraBallTarget
    | StarTarget


type alias Target =
    { position : Vec
    , kind : TargetKind
    }


randomTargets : Int -> Generator (List Target)
randomTargets turns =
    List.map (randomTarget turns) gc.topRowPS
        |> rndCombine
        |> Random.map (List.filterMap identity)


randomTarget : Int -> ( Int, Int ) -> Generator (Maybe Target)
randomTarget turns gp =
    Random.frequency
        ( 25, Random.constant Nothing )
        [ ( 70, rnd1 (initSolidTarget gp) (rndInt 1 (maxHP |> atMost (turns + 3))) |> Random.map Just )
        , ( 5, Random.constant (Just (Target (gpToWorld gp) ExtraBallTarget)) )
        ]


initSolidTarget : ( Int, Int ) -> Int -> Target
initSolidTarget gp hp =
    Target (gpToWorld gp) (SolidTarget hp)


maxHP =
    20


moveTargetDown : Target -> Target
moveTargetDown target =
    { target | position = vecMapY (add (gc.cri.y * 2)) target.position }


canTargetsSafelyMoveDown : List Target -> Bool
canTargetsSafelyMoveDown targets =
    let
        maxGY =
            maximumBy (.position >> .y) targets
                |> Maybe.map (.position >> gpFromWorld >> snd)
                |> Maybe.withDefault -1
    in
    maxGY < (gc.h - 2)


type alias GridConf =
    { ri : Vec
    , w : Int
    , h : Int
    , cri : Vec
    , targetR : Float
    , ballR : Float
    , dx : Float
    , dy : Float
    , aspectRatio : Float
    , topRowPS : List ( Int, Int )
    }


gc : GridConf
gc =
    let
        ( w, h ) =
            ( 7, 10 )

        cw =
            50

        cri =
            vec cw cw

        ri =
            vec (toFloat w * cri.x) (toFloat h * cri.y)

        ar =
            vecApply fdiv ri

        tr =
            cri.x * 0.7

        br =
            tr * 0.6

        cell0Center =
            vecSub cri ri

        topRowPS =
            List.range 0 (w - 1)
                |> List.concatMap (\x -> List.range 1 1 |> List.map (pair x))
    in
    { ri = ri
    , w = w
    , h = h
    , cri = cri
    , targetR = tr
    , ballR = br
    , dx = cell0Center.x
    , dy = cell0Center.y
    , aspectRatio = ar
    , topRowPS = topRowPS
    }


gpToWorld : ( Int, Int ) -> Vec
gpToWorld ( x, y ) =
    let
        { cri, dy, dx } =
            gc
    in
    vec (toFloat x * cri.x * 2 + dx) (toFloat y * cri.y * 2 + dy)


gpFromWorld : Vec -> ( Int, Int )
gpFromWorld { x, y } =
    let
        { cri, dy, dx } =
            gc
    in
    ( (x - dx) / (cri.x * 2), (y - dy) / (cri.y * 2) )
        |> round2


type alias Ball =
    { position : Vec
    , angle : Float
    , speed : Float
    , hue : Float
    , radius : Float
    }


initialBallPosition =
    vec 0 (gc.ri.y - gc.ballR)


ballHue =
    0.15


initBall : Vec -> Float -> Ball
initBall position angle =
    { position = position
    , angle = angle
    , hue = ballHue
    , radius = gc.ballR
    , speed = gc.ballR * 0.9
    }


ballVelocity : Ball -> Vec
ballVelocity ball =
    vecFromRTheta ball.speed ball.angle


setBallPosition : Vec -> Ball -> Ball
setBallPosition p ball =
    { ball | position = p }


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


screenSegments =
    boundingSegFromRadii gc.ri


edges : List Seg
edges =
    let
        { top, right, bottom, left } =
            screenSegments
    in
    [ top, right, bottom, left ]


bottomEdge =
    screenSegments.bottom


isBottomEdge : Seg -> Bool
isBottomEdge edge =
    edge == bottomEdge


type Msg
    = GotDomViewPort Dom.Viewport
    | OnDomResize Int Int
    | OnTick Float
    | PointerDown Bool Vec
    | PointerMoved Vec
    | RestartGameClicked


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        initialSeed =
            seedFrom 4
    in
    ( { turn = -1
      , ballCount = -1
      , targets = []
      , state = GameLost 0

      -- mock game state above ^
      , pointerDown = False
      , prevPointerDown = False
      , pointer = vecZero
      , prevPointer = vecZero
      , frame = 0
      , vri = gc.ri
      , seed = initialSeed
      }
        |> initGame
    , Dom.getViewport |> Task.perform GotDomViewPort
    )


initGame : Model -> Model
initGame model =
    let
        initialBallCount =
            1
    in
    { model
        | ballCount = initialBallCount
        , targets = []
        , state = Running <| TargetsEntering { start = model.frame, ballPosition = initialBallPosition }
        , turn = 1
    }
        |> applyN 8 addNewTargetRowAndIncTurn
        |> identity


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotDomViewPort { viewport } ->
            ( { model | vri = vec viewport.width viewport.height |> vecScale 0.5 }, Cmd.none )

        OnDomResize w h ->
            ( { model | vri = vec (toFloat w) (toFloat h) |> vecScale 0.5 }, Cmd.none )

        OnTick _ ->
            ( updateOnTick model.frame model
                |> incFrame
                |> cachePointer
            , Cmd.none
            )

        PointerDown isDown pointer ->
            ( { model
                | pointerDown = isDown
                , pointer = pointer |> pageToWorld model
              }
            , Cmd.none
            )

        PointerMoved pointer ->
            ( { model
                | pointer = pointer |> pageToWorld model
              }
            , Cmd.none
            )

        RestartGameClicked ->
            ( initGame model, Cmd.none )


pageToWorld : Model -> Vec -> Vec
pageToWorld model pageCord =
    let
        svgRI =
            computeSvgRI model.vri

        svgScale =
            gc.ri.x / svgRI.x
    in
    vecSub pageCord model.vri
        |> vecScale svgScale



--svgToWorld : Model -> Vec -> Vec
--svgToWorld model svgCord =
--    let
--        svgRI =
--            computeSvgRI model
--
--        svgScale =
--            gc.ri.x / svgRI.x
--    in
--    vecSub svgCord svgRI
--        |> vecScale svgScale


cachePointer : Model -> Model
cachePointer model =
    { model | prevPointer = model.pointer, prevPointerDown = model.pointerDown }


updateOnTick : Float -> Model -> Model
updateOnTick frame model =
    case model.state of
        GameLost _ ->
            if model.pointerDown && not model.prevPointerDown then
                initGame model

            else
                model

        Running state ->
            case state of
                TargetsEntering { start, ballPosition } ->
                    if transitionDone start frame then
                        { model | state = Running <| WaitingForInput { ballPosition = ballPosition } }

                    else
                        model

                WaitingForInput { ballPosition } ->
                    if model.pointerDown && not model.prevPointerDown then
                        { model
                            | state =
                                Running <|
                                    DraggingPointer
                                        { dragStartAt = model.pointer |> vecMapY (atMost 0)
                                        , ballPosition = ballPosition
                                        }
                        }

                    else
                        model

                DraggingPointer { dragStartAt, ballPosition } ->
                    if not model.pointerDown then
                        case validInputAngleFromTo dragStartAt model.pointer of
                            Nothing ->
                                { model | state = Running <| WaitingForInput { ballPosition = ballPosition } }

                            Just angle ->
                                let
                                    emitterBall =
                                        initBall ballPosition angle

                                    emitter =
                                        Emitter model.frame
                                            emitterBall
                                            (List.repeat (model.ballCount - 1) emitterBall)
                                in
                                { model | state = Running <| SimTag { mbEmitter = Just emitter, balls = [], floored = [] } }

                    else
                        model

                SimTag sim ->
                    -- check for turn over
                    if sim.mbEmitter == Nothing && sim.balls == [] && areFloorBallsSettled sim.floored then
                        case sim.floored |> List.last |> Maybe.map .position of
                            Nothing ->
                                model

                            Just ballPosition ->
                                -- check for game over
                                let
                                    newModel =
                                        { model
                                            | state =
                                                Running <|
                                                    TargetsEntering
                                                        { start = frame
                                                        , ballPosition = ballPosition
                                                        }
                                        }
                                in
                                if canTargetsSafelyMoveDown model.targets then
                                    newModel
                                        |> addNewTargetRowAndIncTurn

                                else
                                    -- game over : for now re-simulate current turn.
                                    -- newModel
                                    { model | state = GameLost model.frame }
                                        |> addNewTargetRowAndIncTurn

                    else
                        let
                            ( { ebc, targets }, newSim ) =
                                stepSim frame model.targets sim
                        in
                        { model
                            | state = Running <| SimTag newSim
                            , targets = targets
                            , ballCount = model.ballCount + ebc
                        }


stepSim : Float -> List Target -> Sim -> ( { ebc : Int, targets : List Target }, Sim )
stepSim frame targets sim =
    sim.balls
        |> List.mapAccuml updateBall { targets = targets, ebc = 0 }
        |> mapSnd
            (splitBallUpdates
                >> (\{ floored, updated } ->
                        { sim
                            | balls = updated
                            , floored = floored ++ convergeFloorBalls sim.floored
                        }
                            |> stepSimEmitter frame
                   )
            )


validInputAngleFromTo : Vec -> Vec -> Maybe Float
validInputAngleFromTo start current =
    if start.y < current.y then
        vecAngleFromTo current start
            |> clampInputAngle
            |> Just

    else
        Nothing


clampInputAngle =
    clampMO (turns -0.25) (turns 0.24)


incFrame : Model -> Model
incFrame model =
    { model | frame = inc model.frame }


stepSimEmitter : Float -> Sim -> Sim
stepSimEmitter frame sim =
    sim.mbEmitter
        |> Maybe.filter (\{ start } -> frame - start > 10)
        |> Maybe.map
            (\emitter ->
                { sim
                    | balls = emitter.next :: sim.balls
                    , mbEmitter =
                        case emitter.rest of
                            [] ->
                                Nothing

                            n :: r ->
                                Just (Emitter frame n r)
                }
            )
        |> Maybe.withDefault sim


convergeFloorBalls : List Ball -> List Ball
convergeFloorBalls fbs =
    fbs
        |> List.unconsLast
        |> Maybe.map
            (\( last, others ) ->
                List.map (convergeBallTowards last.position) others ++ [ last ]
            )
        |> Maybe.withDefault fbs


convergeBallTowards : Vec -> Ball -> Ball
convergeBallTowards to ball =
    let
        p =
            vecFromTo ball.position to
                |> vecScale 0.1
                |> vecAdd ball.position
    in
    setBallPosition p ball


splitBallUpdates : List BallUpdate -> { floored : List Ball, updated : List Ball }
splitBallUpdates ballUpdates =
    let
        floored =
            List.filterMap
                (\bu ->
                    case bu of
                        BallFloored b ->
                            Just b

                        _ ->
                            Nothing
                )
                ballUpdates

        updated =
            List.filterMap
                (\bu ->
                    case bu of
                        BallMoved b ->
                            Just b

                        _ ->
                            Nothing
                )
                ballUpdates
    in
    { floored = floored, updated = updated }


addNewTargetRowAndIncTurn : Model -> Model
addNewTargetRowAndIncTurn model =
    let
        ( targets, seed ) =
            rndStep ( randomTargets model.turn, model.seed )
    in
    { model
        | targets = targets ++ List.map moveTargetDown model.targets
        , turn = inc model.turn
        , seed = seed
    }


areFloorBallsSettled : List Ball -> Bool
areFloorBallsSettled floorBalls =
    floorBalls
        |> List.unconsLast
        |> Maybe.map
            (\( first, rest ) ->
                List.all (areBallsCloseEnough first) rest
            )
        |> Maybe.withDefault False


areBallsCloseEnough : Ball -> Ball -> Bool
areBallsCloseEnough a b =
    vecLenSqFromTo a.position b.position
        |> eqByAtLeast 0.1 0


type alias BallUpdateAcc =
    { targets : List Target
    , ebc : Int
    }


type BallUpdate
    = BallFloored Ball
    | BallMoved Ball


updateBall : BallUpdateAcc -> Ball -> ( BallUpdateAcc, BallUpdate )
updateBall acc ball =
    let
        velocity =
            ballVelocity ball
                |> vecMapY (add 0.01)
    in
    case detectBallCollision acc.targets velocity ball of
        Nothing ->
            ( acc
            , BallMoved (setBallVelocityAndUpdatePosition velocity ball)
            )

        Just ( collision, ballCollision ) ->
            let
                newBall =
                    resolveBallCollision collision ballCollision velocity ball
            in
            case ballCollision of
                BallEdgeCollision e ->
                    if isBottomEdge e then
                        ( acc
                        , BallFloored newBall
                        )

                    else
                        ( acc
                        , BallMoved newBall
                        )

                BallTargetCollision target ->
                    case target.kind of
                        SolidTarget hp ->
                            if hp <= 1 then
                                ( { acc
                                    | targets = reject (eq target) acc.targets
                                  }
                                , BallMoved newBall
                                )

                            else
                                ( { acc
                                    | targets =
                                        List.setIf (eq target)
                                            { target | kind = SolidTarget (hp - 1) }
                                            acc.targets
                                  }
                                , BallMoved newBall
                                )

                        ExtraBallTarget ->
                            ( { acc
                                | targets = reject (eq target) acc.targets
                                , ebc = acc.ebc + 1
                              }
                            , BallMoved newBall
                            )

                        StarTarget ->
                            ( { acc
                                | targets = reject (eq target) acc.targets
                              }
                            , BallMoved newBall
                            )


resolveBallCollision collision ballCollision velocity ball =
    let
        isSolidCollision =
            case ballCollision of
                BallEdgeCollision _ ->
                    True

                BallTargetCollision target ->
                    case target.kind of
                        SolidTarget _ ->
                            True

                        ExtraBallTarget ->
                            False

                        StarTarget ->
                            False

        newPosition =
            vecAdd ball.position (velocity |> vecScale collision.t)

        newVelocity =
            if isSolidCollision then
                fullyElasticCollisionResponseVelocity collision velocity

            else
                velocity
    in
    setBallPositionAndVelocity newPosition newVelocity ball


type BallCollision
    = BallEdgeCollision Seg
    | BallTargetCollision Target


detectBallCollision : List Target -> Vec -> Ball -> Maybe ( Collision, BallCollision )
detectBallCollision targets velocity ball =
    let
        mc =
            ( ( ball.position, ball.radius ), velocity )

        c1 =
            edges
                |> List.filterMap
                    (\e ->
                        detectMovingCircleAndSegCollision mc e
                            |> Maybe.map (pairTo (BallEdgeCollision e))
                    )

        c2 =
            targets
                |> List.filterMap
                    (\target ->
                        detectMovingCircleAndCircleCollision mc (targetToCircle target)
                            |> Maybe.map (pairTo (BallTargetCollision target))
                    )
    in
    (c1 ++ c2)
        |> minimumBy (fst >> .t)


targetToCircle : Target -> Circle
targetToCircle t =
    ( t.position
    , case t.kind of
        SolidTarget _ ->
            gc.targetR

        ExtraBallTarget ->
            gc.ballR

        StarTarget ->
            gc.ballR
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnTick
        , Browser.Events.onResize OnDomResize
        ]


computeSvgRI : Vec -> Vec
computeSvgRI vri_ =
    let
        ar =
            gc.aspectRatio

        vri =
            vri_
                |> vecScale 0.95

        viewportAR =
            vecApply fdiv vri
    in
    if ar < viewportAR then
        vec (vri.y * ar) vri.y

    else
        vec vri.x (vri.x / ar)


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "height" "100%"
        , E.on "pointerup" (pageXYDecoder |> JD.map (PointerDown False))
        , E.on "pointermove" (pageXYDecoder |> JD.map PointerMoved)
        ]
        [ node "link" [ A.href "styles.css", A.rel "stylesheet" ] []
        , div [ style "position" "relative" ]
            (viewStateContent model)
        ]


viewStateContent : Model -> List (Html Msg)
viewStateContent m =
    let
        svgSkeleton contentView =
            Svg.svg (svgAttrs m.vri)
                [ rect gc.ri [ fillP black ]
                , group []
                    [ viewBallCount m.ballCount
                    , contentView
                    , viewDebugPointer m.pointer |> hideView
                    ]
                ]
    in
    case m.state of
        GameLost start ->
            let
                progress =
                    transitionProgress start m.frame
            in
            [ viewLostStateOverlayTransition progress
            , svgSkeleton (viewTransitioningTargets progress m.targets)
            ]

        Running rs ->
            [ svgSkeleton
                (group []
                    [ viewRunStateTargets m.frame rs m.targets
                    , viewRunningStateContent m.frame m.pointer m.targets rs
                    ]
                )
            ]


viewRunningStateContent : Float -> Vec -> List Target -> RunState -> Svg Msg
viewRunningStateContent frame pointer targets rs =
    case rs of
        TargetsEntering { ballPosition } ->
            viewBallAt ballPosition

        WaitingForInput { ballPosition } ->
            viewBallAt ballPosition

        DraggingPointer { dragStartAt, ballPosition } ->
            group []
                [ viewBallAt ballPosition
                , case validInputAngleFromTo dragStartAt pointer of
                    Nothing ->
                        noView

                    Just angle ->
                        group []
                            [ viewTravelPath frame (ballTravelPath targets ballPosition angle)
                            , viewDebugPoints [ pointer, dragStartAt ]
                            ]
                ]

        SimTag sim ->
            let
                balls =
                    case sim.mbEmitter of
                        Nothing ->
                            sim.balls

                        Just em ->
                            em.next :: sim.balls
            in
            group [] [ viewBalls balls, viewFloorBalls sim.floored ]


svgAttrs : Vec -> List (Attribute Msg)
svgAttrs vri =
    let
        svgRi =
            computeSvgRI vri
    in
    [ T.viewBox -gc.ri.x -gc.ri.y (gc.ri.x * 2) (gc.ri.y * 2)
    , Px.width (svgRi.x * 2)
    , Px.height (svgRi.y * 2)
    , S.fill "none"
    , S.stroke "none"
    , E.on "pointerdown" (pageXYDecoder |> JD.map (PointerDown True))
    , style "touch-action" "none"
    , style "user-select" "none"
    ]


viewBallCount ballCount =
    words (String.fromInt ballCount)
        [ fillH 0.15
        , transform [ translateXY (-gc.ri.x + 20) (-gc.ri.y + 20), scale 3 ]
        ]


viewLostStateOverlayTransition : Float -> Html Msg
viewLostStateOverlayTransition progress =
    div
        [ style "position" "absolute"
        , style "width" "100%"
        , style "height" "100%"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "color" (fromHue 0.15 |> Color.toCssString)
        , style "font-size" "2rem"
        , style "user-select" "none"
        , onClick RestartGameClicked
        , style "background-color"
            (let
                alphaS =
                    progress
                        |> lerp 0 0.9
                        |> mul 100
                        |> round
                        |> String.fromInt
             in
             "rgb(0 0 0 / " ++ alphaS ++ "%)"
            )
        ]
        [ div [ style "font-size" "3rem" ] [ text "Game Over" ]
        , div [] [ text "Tap to Continue" ]
        ]


viewFloorBalls floorBalls =
    case floorBalls of
        [] ->
            noView

        h :: t ->
            viewBalls (h :: (t |> reject (areBallsCloseEnough h)))


viewDebugPointer pointer =
    group []
        [ circle (gc.ballR * 0.5) [ fillH 0.4, transform [ translate pointer ] ]
        , polySeg ( vecZero, pointer ) [ strokeH 0.6 ]
        ]


viewDebugPoints : List Vec -> Svg msg
viewDebugPoints pts =
    group [] (List.map viewDebugPointAt pts)


viewDebugPointAt : Vec -> Svg msg
viewDebugPointAt pt =
    circle (gc.ballR * 0.5) [ fillH 0.4, transform [ translate pt ] ]


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


ballTravelPath : List Target -> Vec -> Float -> List Vec
ballTravelPath targets ballPosition angle =
    let
        ball =
            initBall ballPosition angle
    in
    ballTravelPathHelp
        { targets = targets
        , ebc = 0
        }
        ball
        0
        [ ball.position ]


maxPathLen =
    gc.ri.y * 2.2


ballTravelPathHelp : BallUpdateAcc -> Ball -> Float -> List Vec -> List Vec
ballTravelPathHelp acc ball pathLen path =
    case updateBall acc ball of
        ( nAcc, BallMoved newBall ) ->
            let
                newPathLen =
                    vecLenFromTo ball.position newBall.position + pathLen
            in
            if newPathLen > maxPathLen then
                path

            else
                ballTravelPathHelp nAcc newBall newPathLen (newBall.position :: path)

        _ ->
            path


targetTransitionProgress : RunState -> Float -> Float
targetTransitionProgress rs now =
    case rs of
        TargetsEntering { start } ->
            transitionProgress start now

        _ ->
            1


viewRunStateTargets : Float -> RunState -> List Target -> Svg msg
viewRunStateTargets now rs targets =
    let
        progress =
            targetTransitionProgress rs now
    in
    viewTransitioningTargets progress targets


viewTransitioningTargets : Float -> List Target -> Svg msg
viewTransitioningTargets progress targets =
    let
        dy =
            (1 - progress) * -(gc.cri.y * 2)

        viewTarget target =
            let
                position =
                    target.position |> vecMapY (add dy)
            in
            case target.kind of
                SolidTarget hp ->
                    viewSolidTarget position hp

                ExtraBallTarget ->
                    viewBallAt position

                StarTarget ->
                    viewSolidTarget position -1
    in
    group [] (List.map viewTarget targets)


viewSolidTarget position hp =
    let
        radius =
            gc.targetR

        targetHue =
            toFloat hp / maxHP
    in
    group [ transform [ translate position ] ]
        [ Svg.circle
            [ Px.r radius
            , fillH targetHue
            ]
            []
        , words
            (String.fromInt hp)
            [ fillP black
            , T.fontFamily [ "monospace" ]
            , Px.fontSize (radius * 1.5)

            --, T.fontWeight FontWeightBold
            ]
        ]


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
            in
            viewBallAt p
    in
    do


viewBallAt : Vec -> Svg msg
viewBallAt p =
    let
        radius =
            gc.ballR

        strokeW =
            radius * 0.3

        innerRadius =
            radius - strokeW / 2
    in
    group
        [ strokeH ballHue
        , transform [ translate p ]
        , Px.strokeWidth strokeW
        ]
        [ Svg.circle [ Px.r innerRadius ] []
        ]


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



--noinspection ElmUnusedSymbol


fade o =
    T.opacity (Opacity o)
