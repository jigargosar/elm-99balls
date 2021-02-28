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
import Random exposing (Generator)
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

  - [x] turn relative ball count. don't spend too much time. also check the 99balls game.

  - [x] extract floor balls

  - [x] using anim state for floor balls.

  - [x] simplify target generation

  - [x] try limit based target generation
      - level progression is really good enough.
      - perhaps, we should not touch it all.
      - before any further modifications, record params, do more play testing.

  - test input handling on phone/touch device.
      - Concern: angle flickering on touch release

  - Next milestone ???


# Later

  - add nice `ting` sound on target ball collision

  - Understand exactly how collision computes time `t` and document or return clarifying types.

  - Understand what happens, currently, when objects are already colliding,
    i.e. when objects already intersecting, what is effect on collision response.

  - Invalid states, that are representable.
      - +3 ball outside screen edges / sim never ending.
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


type Model
    = Model Env Game


type alias Env =
    { pointerDown : Bool
    , prevPointerDown : Bool
    , pointer : Vec
    , prevPointer : Vec
    , frame : Float
    , vri : Vec
    }


initialEnvironment : Env
initialEnvironment =
    { pointerDown = False
    , prevPointerDown = False
    , pointer = vecZero
    , prevPointer = vecZero
    , frame = 0
    , vri = gc.ri
    }


type alias Game =
    { turn : Int
    , ballCount : Int
    , targets : List Target
    , state : State
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
    = TargetsEntering { start : Float, ballPosition : Vec }
    | WaitingForInput { ballPosition : Vec }
    | Aiming { dragStartAt : Vec, ballPosition : Vec }
    | Sim_ Sim
    | GameLost Float


type alias Sim =
    { mbEmitter : Maybe Emitter
    , balls : List Ball
    , floorBalls : FloorBalls
    }


type FloorBalls
    = EmptyFloorBalls
    | NonEmptyFloorBalls Vec (List FloorBallAnim)


type alias FloorBallAnim =
    ( Float, Vec )


initFloorBallAnim : Float -> Ball -> FloorBallAnim
initFloorBallAnim now ball =
    ( now, ball.position )


floorBallAnimDone : Float -> FloorBallAnim -> Bool
floorBallAnimDone now ( start, _ ) =
    transitionDone start now


emptyFloorBalls : FloorBalls
emptyFloorBalls =
    EmptyFloorBalls


settledFloorBallsPosition : Float -> FloorBalls -> Maybe Vec
settledFloorBallsPosition now fbs =
    case fbs of
        NonEmptyFloorBalls first anims ->
            if List.all (floorBallAnimDone now) anims then
                Just first

            else
                Nothing

        EmptyFloorBalls ->
            Nothing


addNewFloorBalls : Float -> List Ball -> FloorBalls -> FloorBalls
addNewFloorBalls now balls fbs =
    case fbs of
        EmptyFloorBalls ->
            case List.unconsLast balls of
                Nothing ->
                    EmptyFloorBalls

                Just ( first, others ) ->
                    NonEmptyFloorBalls first.position (others |> List.map (initFloorBallAnim now))

        NonEmptyFloorBalls first anims ->
            NonEmptyFloorBalls first (List.map (initFloorBallAnim now) balls ++ anims)


viewFloorBalls : Float -> FloorBalls -> Svg msg
viewFloorBalls now fbs =
    case fbs of
        EmptyFloorBalls ->
            noView

        NonEmptyFloorBalls first anims ->
            group []
                (viewBallAt first
                    :: (anims
                            |> reject (floorBallAnimDone now)
                            |> List.map (viewFloorBallAnim now first)
                       )
                )


viewFloorBallAnim : Float -> Vec -> FloorBallAnim -> Svg msg
viewFloorBallAnim now toPosition ( start, position ) =
    let
        progress =
            transitionProgress start now

        newPosition =
            position |> vecMapX (\x -> lerp x toPosition.x progress)
    in
    viewBallAt newPosition


initSim : Float -> Vec -> Float -> Int -> Sim
initSim frame ballPosition angle ballCount =
    let
        emitter =
            initEmitter frame ballPosition angle ballCount
    in
    { mbEmitter = Just emitter
    , balls = []
    , floorBalls = emptyFloorBalls
    }


type alias Emitter =
    { start : Float
    , proto : Ball
    , remaining : Int
    }


initEmitter : Float -> Vec -> Float -> Int -> Emitter
initEmitter frame ballPosition angle ballCount =
    let
        proto =
            initBall ballPosition angle
    in
    Emitter frame proto (ballCount - 1)


type TargetKind
    = SolidTarget Int
    | ExtraBallTarget
    | StarTarget


type alias Target =
    { position : Vec
    , kind : TargetKind
    }


initTarget : GP -> TargetKind -> Target
initTarget gp kind =
    Target (gpToWorld gp) kind



--rndGaussLoHi : Float -> Float -> Generator Float
--rndGaussLoHi lo hi =
--    let
--        width =
--            hi - lo
--
--        mean =
--            lo + sd
--
--        sd =
--            width / 2
--    in
--    Random.Float.normal mean sd


randomTargets : Int -> Generator (List Target)
randomTargets turns =
    rnd2 (List.map2 initTarget)
        (shuffle gc.topRowPS)
        (rnd2 (++) (randomSolidTargetKinds turns) randomExtraBallTargetKinds)


bellN : Int -> Generator Float
bellN n =
    rndList n (rndF -1 1)
        |> rnd1 (List.sum >> (\total -> total / toFloat n))


rndExtraBallCount : Generator Int
rndExtraBallCount =
    rndNormal 0 2
        |> rnd1 (abs >> round)


rndSolidTargetCount : Int -> Generator Int
rndSolidTargetCount turns =
    let
        t =
            toFloat turns

        ( mean, sd ) =
            ( (t / 2) |> atMost (toFloat gc.w - 2), 2 )
    in
    rndNormal mean sd
        |> rnd1 (abs >> round >> clamp 1 (gc.w - 1))


rndTargetHealth : Int -> Generator Int
rndTargetHealth turns =
    let
        t =
            toFloat turns

        ( mean, sd ) =
            ( t / 2 |> atMost (maxHP / 2), 5 )
    in
    rndNormal mean sd
        |> rnd1 (clamp 1 (min (t + 1) maxHP) >> round)


randomExtraBallTargetKinds : Generator (List TargetKind)
randomExtraBallTargetKinds =
    rndExtraBallCount |> rnd1 (\i -> List.repeat i ExtraBallTarget)


randomSolidTargetKinds : Int -> Generator (List TargetKind)
randomSolidTargetKinds turns =
    rndSolidTargetCount turns
        |> rndAndThen (\i -> rndList i (rndTargetHealth turns |> rnd1 SolidTarget))


rndNormal m sd =
    bellN 3 |> rnd1 (mul sd >> add m)


maxHP =
    50


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


type alias GP =
    ( Int, Int )


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
    , topRowPS : List GP
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


gpToWorld : GP -> Vec
gpToWorld ( x, y ) =
    let
        { cri, dy, dx } =
            gc
    in
    vec (toFloat x * cri.x * 2 + dx) (toFloat y * cri.y * 2 + dy)


gpFromWorld : Vec -> GP
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


isBottomEdge : Seg -> Bool
isBottomEdge edge =
    edge == screenSegments.bottom


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

        env =
            initialEnvironment
    in
    ( Model env (initGame env.frame initialSeed |> applyN 4 (reStartGame 0))
    , Dom.getViewport |> Task.perform GotDomViewPort
    )


reStartGame : Float -> Game -> Game
reStartGame frame game =
    initGame frame game.seed


initGame : Float -> Seed -> Game
initGame frame seed =
    let
        initialBallCount =
            1
    in
    { ballCount = initialBallCount
    , targets = []
    , state = TargetsEntering { start = frame, ballPosition = initialBallPosition }
    , turn = 1
    , seed = seed
    }
        |> applyN 1 addNewTargetRowAndIncTurn


update : Msg -> Model -> ( Model, Cmd Msg )
update message (Model env game) =
    case message of
        GotDomViewPort { viewport } ->
            ( Model { env | vri = vec viewport.width viewport.height |> vecScale 0.5 }
                game
            , Cmd.none
            )

        OnDomResize w h ->
            ( Model
                { env | vri = vec (toFloat w) (toFloat h) |> vecScale 0.5 }
                game
            , Cmd.none
            )

        OnTick _ ->
            ( Model
                { env
                    | frame = inc env.frame
                    , prevPointer = env.pointer
                    , prevPointerDown = env.pointerDown
                }
                (updateGameOnTick env game)
            , Cmd.none
            )

        PointerDown isDown pointer ->
            ( Model
                { env
                    | pointerDown = isDown
                    , pointer = pointer |> pageToWorld env
                }
                game
            , Cmd.none
            )

        PointerMoved pointer ->
            ( Model
                { env
                    | pointer = pointer |> pageToWorld env
                }
                game
            , Cmd.none
            )

        RestartGameClicked ->
            ( Model env (reStartGame env.frame game), Cmd.none )


pageToWorld : Env -> Vec -> Vec
pageToWorld env pageCord =
    let
        svgRI =
            computeSvgRI env.vri

        svgScale =
            gc.ri.x / svgRI.x
    in
    vecSub pageCord env.vri
        |> vecScale svgScale


initWaitingForInputState : Vec -> State
initWaitingForInputState ballPosition =
    WaitingForInput { ballPosition = ballPosition }


initTargetsEnteringState : Float -> Vec -> State
initTargetsEnteringState start ballPosition =
    TargetsEntering
        { start = start
        , ballPosition = ballPosition
        }


initAimingStateTowards : Vec -> Vec -> State
initAimingStateTowards pointer ballPosition =
    Aiming
        { dragStartAt = pointer |> vecMapY (atMost 0)
        , ballPosition = ballPosition
        }


updateGameOnTick : Env -> Game -> Game
updateGameOnTick { pointer, pointerDown, prevPointerDown, frame } game =
    case game.state of
        GameLost _ ->
            game

        TargetsEntering { start, ballPosition } ->
            if transitionDone start frame then
                { game | state = initWaitingForInputState ballPosition }

            else
                game

        WaitingForInput { ballPosition } ->
            if pointerDown && not prevPointerDown then
                { game | state = initAimingStateTowards pointer ballPosition }

            else
                game

        Aiming { dragStartAt, ballPosition } ->
            if not pointerDown then
                { game
                    | state =
                        case validAimAngleTowards dragStartAt pointer of
                            Nothing ->
                                initWaitingForInputState ballPosition

                            Just angle ->
                                Sim_ (initSim frame ballPosition angle game.ballCount)
                }

            else
                game

        Sim_ sim ->
            case ballPositionOnSimEnd frame sim of
                Just ballPosition ->
                    { game
                        | state =
                            if canTargetsSafelyMoveDown game.targets then
                                initTargetsEnteringState frame ballPosition

                            else
                                GameLost frame
                    }
                        |> addNewTargetRowAndIncTurn

                Nothing ->
                    stepSim frame game sim


ballPositionOnSimEnd : Float -> Sim -> Maybe Vec
ballPositionOnSimEnd now sim =
    let
        turnEnded =
            (sim.mbEmitter == Nothing)
                && (sim.balls == [])
    in
    if turnEnded then
        settledFloorBallsPosition now sim.floorBalls

    else
        Nothing


stepSim : Float -> Game -> Sim -> Game
stepSim frame game sim =
    let
        ( { ballsCollected, targets }, newSim ) =
            stepSimHelp frame game.targets sim
    in
    { game
        | state = Sim_ newSim
        , targets = targets
        , ballCount = game.ballCount + ballsCollected
    }


stepSimHelp : Float -> List Target -> Sim -> ( BallUpdateAcc, Sim )
stepSimHelp frame targets sim =
    sim.balls
        |> List.mapAccuml updateBall { targets = targets, ballsCollected = 0 }
        |> mapSnd
            (splitBallUpdates
                >> (\{ floored, updated } ->
                        { sim
                            | balls = updated
                            , floorBalls = addNewFloorBalls frame floored sim.floorBalls
                        }
                            |> stepSimEmitter frame
                   )
            )


validAimAngleTowards : Vec -> Vec -> Maybe Float
validAimAngleTowards to from =
    if to.y < from.y then
        vecAngleFromTo from to
            |> clampMO (turns -0.25) (turns 0.24)
            |> Just

    else
        Nothing


stepSimEmitter : Float -> Sim -> Sim
stepSimEmitter frame sim =
    sim.mbEmitter
        |> Maybe.filter (\{ start } -> frame - start > 10)
        |> Maybe.map
            (\{ proto, remaining } ->
                { sim
                    | balls = proto :: sim.balls
                    , mbEmitter =
                        if remaining <= 0 then
                            Nothing

                        else
                            Just (Emitter frame proto (remaining - 1))
                }
            )
        |> Maybe.withDefault sim


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


addNewTargetRowAndIncTurn : Game -> Game
addNewTargetRowAndIncTurn game =
    let
        ( targets, seed ) =
            rndStep ( randomTargets game.turn, game.seed )
    in
    { game
        | targets = targets ++ List.map moveTargetDown game.targets
        , turn = inc game.turn
        , seed = seed
    }


type alias BallUpdateAcc =
    { targets : List Target
    , ballsCollected : Int
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
                                ( { acc | targets = remove target acc.targets }
                                , BallMoved newBall
                                )

                            else
                                ( { acc
                                    | targets =
                                        replace target
                                            { target | kind = SolidTarget (hp - 1) }
                                            acc.targets
                                  }
                                , BallMoved newBall
                                )

                        ExtraBallTarget ->
                            ( { acc
                                | targets = remove target acc.targets
                                , ballsCollected = acc.ballsCollected + 1
                              }
                            , BallMoved newBall
                            )

                        StarTarget ->
                            ( { acc
                                | targets = remove target acc.targets
                              }
                            , BallMoved newBall
                            )


resolveBallCollision : Collision -> BallCollision -> Vec -> Ball -> Ball
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
        mc : MovingCircle
        mc =
            ( ( ball.position, ball.radius ), velocity )

        c1 : List ( Collision, BallCollision )
        c1 =
            edges
                |> List.filterMap
                    (\e ->
                        detectMovingCircleAndSegCollision mc e
                            |> Maybe.map (pairTo (BallEdgeCollision e))
                    )

        c2 : List ( Collision, BallCollision )
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
view (Model env game) =
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
            (viewGameContent env game)
        ]


viewGameContent : Env -> Game -> List (Html Msg)
viewGameContent { vri, frame, pointer } g =
    [ viewGameLost frame g
    , Svg.svg (svgAttrs vri)
        [ rect gc.ri [ fillP black ]
        , group []
            [ viewBallCount g.ballCount
            , viewTargets frame g.state g.targets
            , viewStateContent frame pointer g.targets g.state
            , viewDebugPointer pointer |> hideView
            ]
        ]
    ]


viewStateContent : Float -> Vec -> List Target -> State -> Svg Msg
viewStateContent frame pointer targets state =
    case state of
        GameLost _ ->
            noView

        TargetsEntering { ballPosition } ->
            viewBallAt ballPosition

        WaitingForInput { ballPosition } ->
            viewBallAt ballPosition

        Aiming { dragStartAt, ballPosition } ->
            group []
                [ viewBallAt ballPosition
                , case validAimAngleTowards dragStartAt pointer of
                    Nothing ->
                        noView

                    Just angle ->
                        group []
                            [ viewTravelPath frame (ballTravelPath targets ballPosition angle)
                            , viewDebugPoints [ pointer, dragStartAt ]
                            ]
                ]

        Sim_ sim ->
            let
                balls =
                    case sim.mbEmitter of
                        Nothing ->
                            sim.balls

                        Just em ->
                            em.proto :: sim.balls
            in
            group []
                [ viewBalls balls
                , viewFloorBalls frame sim.floorBalls
                ]


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


viewBallCount : Int -> Svg msg
viewBallCount ballCount =
    words (String.fromInt ballCount)
        [ fillH 0.15
        , transform [ translateXY (-gc.ri.x + 20) (-gc.ri.y + 20), scale 3 ]
        ]


viewGameLost : Float -> Game -> Html Msg
viewGameLost now { state } =
    case state of
        GameLost start ->
            viewGameLostHelp (transitionProgress start now)

        _ ->
            noView


viewGameLostHelp : Float -> Html Msg
viewGameLostHelp progress =
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
        , ballsCollected = 0
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


viewTargets : Float -> State -> List Target -> Svg msg
viewTargets now state targets =
    let
        progress =
            case state of
                GameLost start ->
                    transitionProgress start now

                TargetsEntering { start } ->
                    transitionProgress start now

                _ ->
                    1
    in
    viewTargetsHelp progress targets


viewTargetsHelp : Float -> List Target -> Svg msg
viewTargetsHelp progress targets =
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
