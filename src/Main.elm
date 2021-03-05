port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Color exposing (..)
import Html exposing (Attribute, Html, div, node)
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


port playSound : String -> Cmd msg


{-|


# Tasks

  - [x] target kill anim

  - stars

  - mute btn

  - test input handling on phone/touch device.
      - Concern: angle flickering on touch release


# 99 balls game features:

  - mute btn
  - anim
      - bonus star
      - target hit
      - target fall
  - adjust aim path length, when drag distance is smaller.
  - end screen: score/hi-score on over.
  - start screen
  - star/shop gimmick. Not sure. But is part of clone.
  - try using ease for tutorial anim.
  - transitions for ui elements.
  - pause state
  - sound effects.


# Later

  - concern: sim speed seems slow, with 50+ balls

  - try reverse drag for aiming.

  - add nice `ting` sound on target ball collision

  - Understand exactly how collision computes time `t` and document or return clarifying types.

  - Understand what happens, currently, when objects are already colliding,
    i.e. when objects already intersecting, what is effect on collision response.

  - Invalid states, that are representable.
      - +4 sim stuck: ball disappears.
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


type State
    = TargetsEntering { anim : Anim0, ballPosition : Vec }
    | WaitingForInput { ballPosition : Vec }
    | Aiming { dragStartAt : Vec, ballPosition : Vec }
    | Simulating Sim
    | GameLost Anim0


type alias Sim =
    { emitter : Emitter
    , balls : List Ball
    , floorBalls : FloorBalls
    , killSoundIdx : Int
    , killAnims : List KillAnim
    }


type alias KillAnim =
    Anim Vec


initKillAnim : Float -> Vec -> KillAnim
initKillAnim now position =
    let
        killAnimDur =
            40
    in
    initAnim now killAnimDur position


type FloorBalls
    = EmptyFloorBalls
    | NonEmptyFloorBalls Vec (List FloorBallAnim)


type alias FloorBallAnim =
    Anim Vec


initFloorBallAnim : Float -> Ball -> FloorBallAnim
initFloorBallAnim now ball =
    initAnim now transitionDuration ball.position


emptyFloorBalls : FloorBalls
emptyFloorBalls =
    EmptyFloorBalls


settledFloorBallsPosition : Float -> FloorBalls -> Maybe Vec
settledFloorBallsPosition now fbs =
    case fbs of
        NonEmptyFloorBalls first anims ->
            if allAnimDone now anims then
                Just first

            else
                Nothing

        EmptyFloorBalls ->
            Nothing


stepFloorBalls : Float -> List Ball -> FloorBalls -> FloorBalls
stepFloorBalls now newFloored fbs =
    case fbs of
        EmptyFloorBalls ->
            case List.unconsLast newFloored of
                Nothing ->
                    EmptyFloorBalls

                Just ( first, others ) ->
                    NonEmptyFloorBalls first.position (others |> List.map (initFloorBallAnim now))

        NonEmptyFloorBalls first anims ->
            NonEmptyFloorBalls first (List.map (initFloorBallAnim now) newFloored ++ anims)


viewFloorBalls : Float -> FloorBalls -> Svg msg
viewFloorBalls now fbs =
    case fbs of
        EmptyFloorBalls ->
            noView

        NonEmptyFloorBalls first anims ->
            group []
                (viewBallAt first
                    :: (anims
                            |> List.filterMap (viewAnim now (viewFloorBallAnim first))
                       )
                )


viewFloorBallAnim : Vec -> Float -> Vec -> Svg msg
viewFloorBallAnim toPosition progress position =
    let
        newPosition =
            position |> vecMapX (\x -> lerp x toPosition.x progress)
    in
    viewBallAt newPosition


initSim : Float -> Vec -> Float -> Int -> Sim
initSim frame ballPosition angle ballCount =
    { emitter = initEmitter frame ballPosition angle ballCount
    , balls = []
    , floorBalls = emptyFloorBalls
    , killSoundIdx = 0
    , killAnims = []
    }


type Emitter
    = Emitting Ball { start : Float, remaining : Int }
    | EmitterDone


initEmitter : Float -> Vec -> Float -> Int -> Emitter
initEmitter frame ballPosition angle ballCount =
    Emitting (initBall ballPosition angle)
        { start = frame, remaining = ballCount - 1 }


nextEmitterBall : Emitter -> Maybe Ball
nextEmitterBall emitter =
    case emitter of
        Emitting ball _ ->
            Just ball

        _ ->
            Nothing


isEmitterDone : Emitter -> Bool
isEmitterDone =
    eq EmitterDone


stepEmitter : Float -> Emitter -> ( Maybe Ball, Emitter )
stepEmitter now emitter =
    stepEmitterHelp now emitter
        |> Maybe.map (mapFst Just)
        |> Maybe.withDefault ( Nothing, emitter )


stepEmitterHelp : Float -> Emitter -> Maybe ( Ball, Emitter )
stepEmitterHelp now emitter =
    case emitter of
        Emitting ball { start, remaining } ->
            if now - start > 8 then
                Just
                    ( ball
                    , if remaining <= 0 then
                        EmitterDone

                      else
                        Emitting ball { start = now, remaining = remaining - 1 }
                    )

            else
                Nothing

        EmitterDone ->
            Nothing


type TargetKind
    = SolidTarget Int
    | BonusBallTarget
    | StarTarget


type alias Target =
    { position : Vec
    , kind : TargetKind
    }


initTarget : GP -> TargetKind -> Target
initTarget gp kind =
    Target (gpToWorld gp) kind


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
                |> always ( toFloat gc.w / 2, toFloat gc.w / 2 )
    in
    rndNormal mean sd
        |> rnd1 (abs >> round >> clamp 1 (gc.w - 1))


rndTargetHealth : Int -> Generator Int
rndTargetHealth turns =
    let
        t =
            toFloat turns

        ( mean, sd ) =
            ( t / 2 |> clamp 2 (maxHP * 0.7), 5 )
    in
    rndNormal mean sd
        |> rnd1 (abs >> clamp 1 (min (t + 1) maxHP) >> round)


randomExtraBallTargetKinds : Generator (List TargetKind)
randomExtraBallTargetKinds =
    rndExtraBallCount |> rnd1 (\i -> List.repeat i BonusBallTarget)


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
    , cellR : Float
    , cri : Vec
    , targetR : Float
    , ballR : Float
    , ballSpeed : Float
    , dx : Float
    , dy : Float
    , topRowPS : List GP
    }


gc : GridConf
gc =
    let
        ( w, h ) =
            ( 7, 10 )

        cr =
            50

        cri =
            vec cr cr

        ri =
            vec (toFloat w * cri.x) (toFloat h * cri.y)

        tr =
            cr * 0.8

        br =
            tr * 0.6

        ballSpeed =
            br * 0.9

        cell0Center =
            vecSub cri ri

        topRowPS =
            List.range 0 (w - 1)
                |> List.concatMap (\x -> List.range 1 1 |> List.map (pair x))
    in
    { ri = ri
    , w = w
    , h = h
    , cellR = cr
    , cri = cri
    , targetR = tr
    , ballR = br
    , ballSpeed = ballSpeed
    , dx = cell0Center.x
    , dy = cell0Center.y
    , topRowPS = topRowPS
    }


type alias WorldConf =
    { ri : Vec
    , header : { ri : Vec, c : Vec }
    , footer : { ri : Vec, c : Vec }
    , restartBtn : { ri : Vec, c : Vec }
    }


wc : WorldConf
wc =
    let
        headerRI =
            vec gc.ri.x gc.cellR

        footerRI =
            headerRI

        ri =
            gc.ri |> vecMapY (add (headerRI.y + footerRI.y))

        header =
            { ri = headerRI, c = vec 0 -(gc.ri.y + headerRI.y) }

        footer =
            { ri = footerRI, c = vec 0 (gc.ri.y + footerRI.y) }
    in
    { ri = ri
    , header = header
    , footer = footer
    , restartBtn = { ri = gc.cri, c = vec (-header.ri.x + gc.cellR * 2) 0 }
    }


aspectRatioFromRI =
    vecApply fdiv


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
    , speed = gc.ballSpeed
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
    ( Model env
        (initGame env.frame initialSeed
            |> applyN 4 (reStartGame 0)
        )
    , Dom.getViewport |> Task.perform GotDomViewPort
    )


reStartGame : Float -> Game -> Game
reStartGame frame game =
    initGame frame game.seed


initGame : Float -> Seed -> Game
initGame now seed =
    { ballCount = 1
    , targets = []
    , state =
        initTargetsEnteringState now initialBallPosition
            |> always (initGameLost now)
    , turn = 0
    , seed = seed
    }
        |> applyN 8 incTurnThenAddTargetRow


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
            let
                ( newGame, cmd ) =
                    updateGameOnTick env game
            in
            ( Model
                { env
                    | frame = inc env.frame
                    , prevPointer = env.pointer
                    , prevPointerDown = env.pointerDown
                }
                newGame
            , cmd
            )

        PointerDown isDown pointer ->
            ( Model
                { env
                    | pointerDown = isDown
                    , pointer = pointer |> pageToWorld env
                }
                game
            , Cmd.batch []
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
        svgScale =
            wc.ri.x / (svgRIFromBrowserViewportRI env.vri).x
    in
    vecSub pageCord env.vri
        |> vecScale svgScale


initWaitingForInputState : Vec -> State
initWaitingForInputState ballPosition =
    WaitingForInput { ballPosition = ballPosition }


initTargetsEnteringState : Float -> Vec -> State
initTargetsEnteringState now ballPosition =
    TargetsEntering
        { anim = initAnim0 now transitionDuration
        , ballPosition = ballPosition
        }


initAimingTowardsState : Vec -> Vec -> State
initAimingTowardsState pointer ballPosition =
    Aiming
        { dragStartAt = pointer |> vecMapY (atMost 0)
        , ballPosition = ballPosition
        }


initGameLost : Float -> State
initGameLost now =
    GameLost (initAnim0 now transitionDuration)


updateGameOnTick : Env -> Game -> ( Game, Cmd msg )
updateGameOnTick { pointer, pointerDown, prevPointerDown, frame } game =
    case game.state of
        GameLost _ ->
            ( game, Cmd.none )

        TargetsEntering { anim, ballPosition } ->
            ( if isAnimDone frame anim then
                { game | state = initWaitingForInputState ballPosition }

              else
                game
            , Cmd.none
            )

        WaitingForInput { ballPosition } ->
            ( if pointerDown && not prevPointerDown then
                { game | state = initAimingTowardsState pointer ballPosition }

              else
                game
            , Cmd.none
            )

        Aiming { dragStartAt, ballPosition } ->
            ( if not pointerDown then
                { game
                    | state =
                        case validAimAngleTowards dragStartAt pointer of
                            Nothing ->
                                initWaitingForInputState ballPosition

                            Just angle ->
                                Simulating (initSim frame ballPosition angle game.ballCount)
                }

              else
                game
            , Cmd.none
            )

        Simulating sim ->
            case ballPositionOnSimEnd frame sim of
                Just ballPosition ->
                    ( { game
                        | state =
                            if canTargetsSafelyMoveDown game.targets then
                                initTargetsEnteringState frame ballPosition

                            else
                                initGameLost frame
                      }
                        |> incTurnThenAddTargetRow
                    , Cmd.none
                    )

                Nothing ->
                    stepSim frame game sim


ballPositionOnSimEnd : Float -> Sim -> Maybe Vec
ballPositionOnSimEnd now sim =
    let
        simDone =
            isEmitterDone sim.emitter && (sim.balls == [])
    in
    if simDone then
        settledFloorBallsPosition now sim.floorBalls

    else
        Nothing


stepSim : Float -> Game -> Sim -> ( Game, Cmd msg )
stepSim frame game sim =
    let
        ( acc, newBalls ) =
            stepSimBalls game.targets sim.balls

        ( emittedBall, newEmitter ) =
            stepEmitter frame sim.emitter

        newKillSoundIdx =
            if acc.solidTargetsKilled == [] then
                sim.killSoundIdx

            else
                1 + sim.killSoundIdx
    in
    ( { game
        | state =
            Simulating
                { emitter = newEmitter
                , balls = Maybe.cons emittedBall newBalls.moved
                , floorBalls = stepFloorBalls frame newBalls.floored sim.floorBalls
                , killSoundIdx = newKillSoundIdx
                , killAnims = stepKillAnims frame acc.solidTargetsKilled sim.killAnims
                }
        , targets = acc.targets
        , ballCount = acc.bonusBallsCollected + game.ballCount
      }
    , Cmd.batch
        [ cmdIf (acc.solidTargetHits > 0) (playSound "hit")
        , cmdIf (acc.bonusBallsCollected >= 1) (playSound "bonus_hit")
        , cmdIf (newKillSoundIdx > sim.killSoundIdx) (playKillSound newKillSoundIdx)
        , cmdIf (emittedBall /= Nothing) (playSound "shoot")
        ]
    )


stepKillAnims : Float -> List Vec -> List KillAnim -> List KillAnim
stepKillAnims frame newKillPositions killAnims =
    List.map (initKillAnim frame) newKillPositions ++ killAnims


playKillSound : Int -> Cmd msg
playKillSound totalKills =
    playSound ("kill_" ++ String.fromInt (atMost 8 totalKills))


type alias UpdatedBalls =
    { floored : List Ball, moved : List Ball }


stepSimBalls : List Target -> List Ball -> ( BallUpdateAcc, UpdatedBalls )
stepSimBalls targets =
    List.foldl
        (\ball ( acc, newBalls ) ->
            updateBall acc ball
                |> mapSnd
                    (\ballUpdate ->
                        case ballUpdate of
                            BallMoved b ->
                                { newBalls | moved = b :: newBalls.moved }

                            BallFloored b ->
                                { newBalls | floored = b :: newBalls.floored }
                    )
        )
        ( initBallUpdateAcc targets, { floored = [], moved = [] } )


validAimAngleTowards : Vec -> Vec -> Maybe Float
validAimAngleTowards to from =
    if to.y < from.y then
        vecAngleFromTo from to
            |> clampMO (turns -0.25) (turns 0.24)
            |> Just

    else
        Nothing


incTurnThenAddTargetRow : Game -> Game
incTurnThenAddTargetRow game =
    let
        turn =
            game.turn + 1

        ( targets, seed ) =
            rndStep ( randomTargets turn, game.seed )
    in
    { game
        | targets = targets ++ List.map moveTargetDown game.targets
        , turn = turn
        , seed = seed
    }


type alias BallUpdateAcc =
    { targets : List Target
    , bonusBallsCollected : Int
    , solidTargetHits : Int
    , solidTargetsKilled : List Vec
    }


initBallUpdateAcc : List Target -> BallUpdateAcc
initBallUpdateAcc targets =
    { targets = targets
    , bonusBallsCollected = 0
    , solidTargetHits = 0
    , solidTargetsKilled = []
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
                        ( acc, BallFloored newBall )

                    else
                        ( acc, BallMoved newBall )

                BallTargetCollision target ->
                    case target.kind of
                        SolidTarget hp ->
                            if hp <= 1 then
                                ( { acc
                                    | targets = remove target acc.targets
                                    , solidTargetsKilled = target.position :: acc.solidTargetsKilled
                                  }
                                , BallMoved newBall
                                )

                            else
                                ( { acc
                                    | targets =
                                        replace target
                                            { target | kind = SolidTarget (hp - 1) }
                                            acc.targets
                                    , solidTargetHits = acc.solidTargetHits + 1
                                  }
                                , BallMoved newBall
                                )

                        BonusBallTarget ->
                            ( { acc
                                | targets = remove target acc.targets
                                , bonusBallsCollected = acc.bonusBallsCollected + 1
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

                        BonusBallTarget ->
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

        BonusBallTarget ->
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


svgRIFromBrowserViewportRI : Vec -> Vec
svgRIFromBrowserViewportRI bri =
    let
        worldAR =
            aspectRatioFromRI wc.ri

        vri =
            bri |> vecScale 1

        viewportAR =
            aspectRatioFromRI vri
    in
    if worldAR < viewportAR then
        vec (vri.y * worldAR) vri.y

    else
        vec vri.x (vri.x / worldAR)


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
            |> always noView
        , viewGame env game
        ]


viewGame : Env -> Game -> Html Msg
viewGame { vri, frame, pointer } g =
    Svg.svg (svgAttrs vri)
        [ rect wc.ri [ fillP black ]
        , group []
            [ viewHeader g.turn
            , viewFooter g.ballCount

            -- ^--^ draw order matters, when showing aim/debug points
            , viewTargets frame g.state g.targets
            , viewState frame pointer g.turn g.targets g.state
            , viewDebugPointer pointer |> hideView
            ]
        ]


viewHeader : Int -> Svg Msg
viewHeader turn =
    group [ transform [ translate wc.header.c ] ]
        [ rect wc.header.ri [ fillP darkCharcoal ]
        , words (String.fromInt turn)
            [ fillP white, transform [ scale 4 ], T.fontWeight FontWeightBold ]
        , group [ transform [ translate wc.restartBtn.c ] ]
            [ group [ transform [ scale 4 ], fillP white ] [ restartIcon ]
            , rect wc.restartBtn.ri
                [ S.pointerEvents "fill"
                , S.cursor "pointer"
                , alwaysPreventDefaultOn "click" (JD.succeed RestartGameClicked)
                ]
            ]
        ]


viewFooter : Int -> Svg msg
viewFooter ballCount =
    group [ transform [ translate wc.footer.c ] ]
        [ rect wc.footer.ri [ fillP <| hsl 0.07 0.8 0.5 ]
        , circle gc.ballR [ fillP white, transform [ translateXY -(gc.cellR * 1.8) 0 ] ]
        , words (String.fromInt ballCount)
            [ fillP white, transform [ scale 4 ], T.fontWeight FontWeightBold ]
        ]


viewState : Float -> Vec -> Int -> List Target -> State -> Svg Msg
viewState now pointer turn targets state =
    case state of
        GameLost anim ->
            let
                progress =
                    clampedAnimProgress now anim
            in
            group [ onClick RestartGameClicked ]
                [ rect wc.ri [ fillP black, fade (progress |> lerp 0 0.9) ]
                , group
                    [ fillH 0.14
                    , fillH 0.14
                    , fade progress
                    ]
                    [ words "Game Over" [ transform [ translateXY 0 -50, scale 5 ] ]
                    , words "Tap to Continue" [ transform [ translateXY 0 50, scale 3 ] ]
                    ]
                ]

        TargetsEntering { ballPosition } ->
            viewBallAt ballPosition

        WaitingForInput { ballPosition } ->
            group []
                [ viewBallAt ballPosition
                , if turn == 1 then
                    viewTutorial 0 now

                  else
                    noView
                ]

        Aiming { dragStartAt, ballPosition } ->
            group []
                [ viewBallAt ballPosition
                , if turn == 1 then
                    viewTutorial 0 now

                  else
                    noView
                , case validAimAngleTowards dragStartAt pointer of
                    Nothing ->
                        noView

                    Just angle ->
                        group []
                            [ viewTravelPath now (ballTravelPath targets ballPosition angle)
                            , viewDebugPoints [ pointer, dragStartAt ]
                            ]
                ]

        Simulating sim ->
            group []
                [ viewBalls (Maybe.cons (nextEmitterBall sim.emitter) sim.balls)
                , viewFloorBalls now sim.floorBalls
                , viewKillAnims now sim.killAnims
                ]


viewKillAnims : Float -> List KillAnim -> Svg msg
viewKillAnims now killAnimations =
    let
        viewKillAnim : Float -> Vec -> Svg msg
        viewKillAnim progress position =
            let
                s =
                    if progress < 0.2 then
                        progress * -(gc.targetR * 2)

                    else
                        (-(gc.targetR * 2) * 0.2) + (progress - 0.2) * (gc.targetR * 6)

                animPosition : Vec
                animPosition =
                    vecAdd position (vec 0 1 |> vecScale s)
            in
            group
                [ transform [ translate animPosition, Rotate (progress * -45) 0 0 ]
                , fade (lerp 2.5 0 progress)
                ]
                [ Svg.circle
                    [ Px.r gc.targetR
                    , fillH 0
                    , transform
                        [ SkewX (wave 0.5 progress * 15 - 7)
                        , SkewY (wave 0.5 progress * 15 - 7)
                        ]
                    ]
                    []
                ]
    in
    group [] (List.filterMap (viewAnim now viewKillAnim) killAnimations)


viewTutorial : Float -> Float -> Svg msg
viewTutorial start now =
    let
        dur =
            transitionDuration * 4

        elapsed =
            fmodBy (dur * 3) (now - start |> atLeast 0)

        progress =
            fmodBy dur elapsed / dur

        phase =
            elapsed / dur |> floor

        handTravelPath =
            [ vecZero, vec 50 100, vec -50 100 ]

        currHandPosition =
            handTravelPath
                |> List.getAt phase
                |> Maybe.withDefault vecZero

        nextHandPosition =
            handTravelPath
                |> List.getAt (phase + 1)
                |> Maybe.orElse (List.head handTravelPath)
                |> Maybe.withDefault vecZero

        handPosition =
            vecFromTo currHandPosition nextHandPosition
                |> vecScale progress
                |> vecAdd currHandPosition

        viewHand =
            group [ transform [ translate handPosition ] ]
                [ group [ Px.strokeWidth 2, strokeP white ]
                    [ circle (gc.ballR * 0.6) []
                    , circle (gc.ballR * 0.4) []
                    ]
                , handIcon
                    [ fillP white
                    , transform [ scale 0.25, Scale 0.8 1, translateXY -80 0 ]
                    ]
                ]

        viewStartingHandPosition =
            circle (gc.ballR * 0.5) [ fillP white ]
    in
    group
        [ transform [ translateXY 0 (gc.ri.y * 0.4) ]
        , fade 0.6
        ]
        [ case phase of
            0 ->
                noView

            1 ->
                noView

            _ ->
                noView
        , viewTravelPath now [ handPosition |> vecNegate |> vecScale 2, vecZero ]
        , viewHand
        , viewStartingHandPosition
        ]


handIcon aa =
    Svg.path
        (S.d
            """M192.231,104.082V102c0-12.407-10.094-22.5-22.5-22.5c-2.802,0-5.484,0.519-7.961,1.459
C159.665,70.722,150.583,63,139.731,63c-2.947,0-5.76,0.575-8.341,1.61C128.667,55.162,119.624,48,109.231,48
c-2.798,0-5.496,0.541-8,1.516V22.5c0-12.407-10.094-22.5-22.5-22.5s-22.5,10.093-22.5,22.5v66.259
c-3.938-5.029-8.673-9.412-14.169-11.671c-6.133-2.52-12.587-2.219-18.667,0.872c-11.182,5.686-15.792,19.389-10.277,30.548
l27.95,56.563c0.79,1.552,19.731,38.008,54.023,38.008h40c31.54,0,57.199-25.794,57.199-57.506l-0.031-41.491H192.231z
M135.092,188.079h-40c-24.702,0-40.091-28.738-40.646-29.796l-27.88-56.42c-1.924-3.893-0.33-8.519,3.629-10.532
c2.182-1.11,4.081-1.223,6.158-0.372c8.281,3.395,16.41,19.756,19.586,29.265l2.41,7.259l12.883-4.559V22.5
c0-4.136,3.364-7.5,7.5-7.5s7.5,3.364,7.5,7.5V109h0.136h14.864h0.136V71c0-4.187,3.748-8,7.864-8c4.262,0,8,3.505,8,7.5v15v26h15
v-26c0-4.136,3.364-7.5,7.5-7.5s7.5,3.364,7.5,7.5V102v16.5h15V102c0-4.136,3.364-7.5,7.5-7.5s7.5,3.364,7.5,7.5v10.727h0.035
l0.025,32.852C177.291,169.014,158.36,188.079,135.092,188.079z
"""
            :: aa
        )
        []


restartIcon =
    Svg.path
        (S.d
            """M12.083,1.887c-0.795-0.794-1.73-1.359-2.727-1.697v2.135c0.48,0.239,0.935,0.55,1.334,0.95
c1.993,1.994,1.993,5.236,0,7.229c-1.993,1.99-5.233,1.99-7.229,0c-1.991-1.995-1.991-5.235,0-7.229
C3.466,3.269,3.482,3.259,3.489,3.25h0.002l1.181,1.179L4.665,0.685L0.923,0.68l1.176,1.176C2.092,1.868,2.081,1.88,2.072,1.887
c-2.763,2.762-2.763,7.243,0,10.005c2.767,2.765,7.245,2.765,10.011,0C14.844,9.13,14.847,4.649,12.083,1.887z
"""
            :: [ transform [ translateXY -(14.155 / 2) -(14.155 / 2) ] ]
        )
        []


svgAttrs : Vec -> List (Attribute Msg)
svgAttrs vri =
    let
        svgRi =
            svgRIFromBrowserViewportRI vri
    in
    [ viewBoxFromRI wc.ri
    , Px.width (svgRi.x * 2)
    , Px.height (svgRi.y * 2)
    , S.fill "none"
    , S.stroke "none"
    , E.on "pointerdown" (pageXYDecoder |> JD.map (PointerDown True))
    , style "touch-action" "none"
    , style "user-select" "none"
    , style "display" "block"
    ]


viewBoxFromRI ri =
    T.viewBox -ri.x -ri.y (ri.x * 2) (ri.y * 2)


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
    ballTravelPathHelp (initBallUpdateAcc targets)
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
                GameLost anim ->
                    clampedAnimProgress now anim

                TargetsEntering { anim } ->
                    clampedAnimProgress now anim

                _ ->
                    1
    in
    viewTargetsHelp now progress targets


viewTargetsHelp : Float -> Float -> List Target -> Svg msg
viewTargetsHelp now progress targets =
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

                BonusBallTarget ->
                    viewBonusBall now position

                StarTarget ->
                    viewSolidTarget position -1
    in
    group [] (List.map viewTarget targets)


viewBonusBall now position =
    let
        ( ndx, ndy ) =
            ( wave 80 now |> normToNegNorm, zigZag 90 now |> normToNegNorm )

        dxy =
            vec ndx ndy
                |> vecScale (gc.ballR * 0.3)

        p2 =
            vecAdd position dxy
    in
    viewBallAt p2


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



-- ANIM


type Anim a
    = Anim { start : Float, duration : Float, data : a }


type alias Anim0 =
    Anim ()


initAnim : Float -> Float -> a -> Anim a
initAnim now duration data =
    Anim { start = now, duration = duration, data = data }


initAnim0 : Float -> Float -> Anim0
initAnim0 now duration =
    initAnim now duration ()


allAnimDone : Float -> List (Anim a) -> Bool
allAnimDone now =
    List.all (isAnimDone now)


isAnimDone : Float -> Anim a -> Bool
isAnimDone now (Anim { start, duration }) =
    now - start >= duration


viewAnim : Float -> (Float -> a -> b) -> Anim a -> Maybe b
viewAnim now fn ((Anim { data }) as anim) =
    animProgress now anim
        |> Maybe.map (\progress -> fn progress data)


animProgress : Float -> Anim a -> Maybe Float
animProgress now anim =
    let
        progress =
            unsafe__AnimProgress now anim
    in
    if progress >= 0 && progress < 1 then
        Just progress

    else
        Nothing


unsafe__AnimProgress : Float -> Anim a -> Float
unsafe__AnimProgress now (Anim { start, duration }) =
    (now - start) / duration


clampedAnimProgress : Float -> Anim a -> Float
clampedAnimProgress now anim =
    unsafe__AnimProgress now anim |> clamp 0 1
