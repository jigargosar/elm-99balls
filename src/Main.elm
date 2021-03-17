port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Color exposing (..)
import FontAwesome.Icon exposing (Icon)
import FontAwesome.Solid as Icon
import Html exposing (Attribute, Html, div, node)
import Html.Attributes as A exposing (style)
import Html.Events as E exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Generator, Seed)
import Svg exposing (Svg)
import Svg.Attributes as S
import Task
import Time
import Tuple exposing (pair)
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types exposing (..)
import Util exposing (..)


port playSound : String -> Cmd msg


port saveStars : Int -> Cmd msg


{-|


# Tasks

  - mute btn
      - try introducing session

  - start screen with logo/play/shop buttons.
      - star & hi score stats.

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
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flags =
    { stars : Int }


type Model
    = Model Env Page


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


type Page
    = StartPage Start
    | GamePage Game_


initStartPage : Int -> Seed -> Page
initStartPage stars seed =
    StartPage (Start stars seed)


type alias Start =
    { stars : Int, seed : Seed }


type Game_
    = Running Game
    | Paused Game
    | Over Game


type alias Game =
    { transit : Anim0
    , ballPosition : Vec
    , turn : Int
    , targets : List Target
    , ballCount : Int
    , stars : Int
    , state : State
    , seed : Seed
    }


type State
    = Aiming (Maybe Vec)
    | Simulating Sim


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


addFloorBalls : Float -> List Ball -> FloorBalls -> FloorBalls
addFloorBalls now newFloored fbs =
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
                (viewBall first
                    :: (anims
                            |> List.filterMap (viewAnim now (viewFloorBall first))
                       )
                )


viewFloorBall : Vec -> Float -> Vec -> Svg msg
viewFloorBall toPosition progress position =
    let
        newPosition =
            position |> vecMapX (\x -> lerp x toPosition.x progress)
    in
    viewBall newPosition


initSim : Emitter -> Sim
initSim emitter =
    { emitter = emitter
    , balls = []
    , floorBalls = emptyFloorBalls
    , killSoundIdx = 0
    , killAnims = []
    }


type Emitter
    = Emitting Ball { start : Float, remaining : Int }
    | EmitterDone


initEmitter : Float -> Ball -> Int -> Emitter
initEmitter frame ball ballCount =
    Emitting ball { start = frame, remaining = ballCount - 1 }


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
    | BonusBallTarget Float
    | StarTarget Float


type alias Target =
    { position : Vec
    , kind : TargetKind
    }


initTarget : GP -> TargetKind -> Target
initTarget gp kind =
    { position = gpToWorld gp
    , kind = kind
    }


initTopRowTargets : Int -> Seed -> ( List Target, Seed )
initTopRowTargets turn seed =
    rndStep ( randomTopRowTargets turn, seed )


randomTopRowTargets : Int -> Generator (List Target)
randomTopRowTargets turns =
    rnd2 (List.map2 initTarget)
        (shuffle gc.topRowPS)
        (rnd3 (\solids balls stars -> solids ++ balls ++ stars)
            (rndLenList rndSolidTargetCount (rndSolidTargetKind turns))
            (rndLenList rndBonusBallCount rndBonusBallTargetKind)
            (rndLenList rndStarCount rndStarTargetKind)
        )


rndBonusBallCount : Generator Int
rndBonusBallCount =
    rndBell_ 0 2 |> rnd1 (abs >> round)


rndStarCount : Generator Int
rndStarCount =
    rndBellMO 6 0 2 |> rnd1 (atLeast 0 >> round)


rndSolidTargetCount : Generator Int
rndSolidTargetCount =
    let
        ( mean, sd ) =
            ( toFloat gc.w / 2, toFloat gc.w / 2 )
    in
    rndBell_ mean sd
        |> rnd1 (abs >> round >> clamp 1 (gc.w - 1))


rndTargetHealth : Int -> Generator Int
rndTargetHealth turns =
    let
        t =
            toFloat turns

        ( mean, sd ) =
            ( t / 2 |> clamp 2 (maxHP * 0.7), 5 )
    in
    rndBell_ mean sd
        |> rnd1 (abs >> clamp 1 (min (t + 1) maxHP) >> round)


rndBonusBallTargetKind : Generator TargetKind
rndBonusBallTargetKind =
    rndF 0 100 |> rnd1 BonusBallTarget


rndStarTargetKind : Generator TargetKind
rndStarTargetKind =
    rndF 0 100 |> rnd1 StarTarget


rndSolidTargetKind : Int -> Generator TargetKind
rndSolidTargetKind turns =
    rndTargetHealth turns |> rnd1 SolidTarget


rndBell_ : Float -> Float -> Generator Float
rndBell_ =
    rndBellMO 3


maxHP : number
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
    , edges : List Seg
    , bottomEdge : Seg
    , w : Int
    , h : Int
    , cellR : Float
    , cri : Vec
    , targetR : Float
    , bonusR : Float
    , ballR : Float
    , ballSpeed : Float
    , gravity : Vec
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

        targetR =
            cr * 0.8

        bonusR =
            targetR * 0.8

        ballR =
            targetR * 0.6

        ballSpeed =
            ballR * 0.9

        leftTopCellCenter =
            vecSub cri ri

        topRowPS =
            List.range 0 (w - 1) |> List.map (pairTo 1)

        { top, right, bottom, left } =
            boundingSegFromRadii ri
    in
    { ri = ri
    , edges = [ top, right, bottom, left ]
    , bottomEdge = bottom
    , w = w
    , h = h
    , cellR = cr
    , cri = cri
    , targetR = targetR
    , bonusR = bonusR
    , ballR = ballR
    , ballSpeed = ballSpeed
    , gravity = vec 0 0.01
    , dx = leftTopCellCenter.x
    , dy = leftTopCellCenter.y
    , topRowPS = topRowPS
    }


type alias WorldConf =
    { ri : Vec
    , header : { ri : Vec, c : Vec }
    , footer : { ri : Vec, c : Vec }
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
    }


aspectRatioFromRI : Vec -> Float
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
    }


ballToCircle : Ball -> Circle
ballToCircle { position } =
    ( position, gc.ballR )


initialBallPosition : Vec
initialBallPosition =
    vec 0 (gc.ri.y - gc.ballR)


bonusHue : Float
bonusHue =
    0.15


initBall : Vec -> Float -> Ball
initBall position angle =
    { position = position
    , angle = angle
    }


ballVelocity : Ball -> Vec
ballVelocity ball =
    vecFromRTheta gc.ballSpeed ball.angle


setBallPositionAndVelocity : Vec -> Vec -> Ball -> Ball
setBallPositionAndVelocity p v ball =
    { ball | position = p, angle = vecAngle v }


setBallVelocityAndUpdatePosition : Vec -> Ball -> Ball
setBallVelocityAndUpdatePosition rawVelocity ball =
    let
        angle =
            vecAngle rawVelocity

        velocity =
            vecFromRTheta gc.ballSpeed angle
    in
    { ball
        | position = vecAdd ball.position velocity
        , angle = angle
    }


type Msg
    = OnDomResize Int Int
    | OnTick
    | PointerDown Bool Vec
    | PointerMoved Vec
    | RestartGameClicked
    | PauseGameClicked
    | ResumeGameClicked
    | HomeBtnClicked
    | StartGameClicked


init : Flags -> ( Model, Cmd Msg )
init { stars } =
    let
        initialSeed =
            seedFrom 5

        env =
            initialEnvironment

        page =
            initGamePage env.frame stars initialSeed

        --|> always (StartPage { stars = stars, seed = initialSeed })
    in
    ( Model env page
    , Dom.getViewport
        |> Task.perform
            (\{ viewport } ->
                OnDomResize (round viewport.width) (round viewport.height)
            )
    )


initGamePage : Float -> Int -> Seed -> Page
initGamePage now stars seed =
    let
        game =
            case initGame now stars seed of
                Running g ->
                    g
                        |> spoofBallCountBy 0
                        |> spoofTurns 7

                --|> (pauseGame >> fst)
                g ->
                    g
    in
    GamePage game


spoofBallCountBy : Int -> Game -> Game
spoofBallCountBy count game =
    { game | ballCount = game.ballCount + count |> atLeast 1 }


spoofTurns : Int -> Game -> Game_
spoofTurns n game =
    if n <= 0 then
        Running game

    else
        case game |> updateGameOnSimEnd 0 initialBallPosition |> fst of
            Running newGame ->
                spoofTurns (n - 1) newGame

            game_ ->
                game_


initGame : Float -> Int -> Seed -> Game_
initGame now stars seed0 =
    let
        turn =
            1

        ( targets, seed ) =
            initTopRowTargets turn seed0
    in
    Running
        { transit = initTransit now
        , ballPosition = initialBallPosition
        , ballCount = 1
        , targets = targets
        , stars = stars
        , state = Aiming Nothing
        , turn = turn
        , seed = seed
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update message (Model env page) =
    case message of
        OnDomResize w h ->
            ( Model
                { env | vri = vec (toFloat w) (toFloat h) |> vecScale 0.5 }
                page
            , Cmd.none
            )

        OnTick ->
            let
                ( newPage, pageCmd ) =
                    case page of
                        GamePage game ->
                            updateGameOnTick env game
                                |> mapFst GamePage

                        _ ->
                            ( page, Cmd.none )
            in
            ( Model
                { env
                    | frame = inc env.frame
                    , prevPointer = env.pointer
                    , prevPointerDown = env.pointerDown
                }
                newPage
            , pageCmd
            )

        PointerDown isDown pointer ->
            ( Model
                { env
                    | pointerDown = isDown
                    , pointer = pointer |> pageToWorld env
                }
                page
            , Cmd.batch []
            )

        PointerMoved pointer ->
            ( Model
                { env
                    | pointer = pointer |> pageToWorld env
                }
                page
            , Cmd.none
            )

        RestartGameClicked ->
            let
                restartGame game =
                    ( Model env (initGamePage env.frame game.stars game.seed), playSound "btn" )
            in
            case page of
                GamePage (Running game) ->
                    restartGame game

                GamePage (Paused game) ->
                    restartGame game

                GamePage (Over game) ->
                    restartGame game

                _ ->
                    ( Model env page, Cmd.none )

        ResumeGameClicked ->
            case page of
                GamePage (Paused game) ->
                    ( Model env (GamePage (Running game)), playSound "btn" )

                _ ->
                    ( Model env page, Cmd.none )

        PauseGameClicked ->
            case page of
                GamePage game ->
                    pauseGame game |> mapFst (GamePage >> Model env)

                _ ->
                    ( Model env page, Cmd.none )

        StartGameClicked ->
            case page of
                StartPage start ->
                    ( Model env (initGamePage env.frame start.stars start.seed)
                    , playSound "btn"
                    )

                _ ->
                    ( Model env page, Cmd.none )

        HomeBtnClicked ->
            case page of
                GamePage (Paused g) ->
                    ( Model env (initStartPage g.stars g.seed), playSound "btn" )

                GamePage (Over g) ->
                    ( Model env (initStartPage g.stars g.seed), playSound "btn" )

                _ ->
                    ( Model env page, Cmd.none )


pauseGame : Game_ -> ( Game_, Cmd msg )
pauseGame game_ =
    case game_ of
        Running game ->
            ( Paused game, playSound "btn" )

        _ ->
            ( game_, Cmd.none )


pageToWorld : Env -> Vec -> Vec
pageToWorld env pageCord =
    let
        svgScale =
            wc.ri.x / (svgRIFromBrowserViewportRI env.vri).x
    in
    vecSub pageCord env.vri
        |> vecScale svgScale


initAimingState : Vec -> State
initAimingState pointer =
    Aiming <| Just (pointer |> vecMapY (atMost 0))


updateGameOnTick : Env -> Game_ -> ( Game_, Cmd msg )
updateGameOnTick { pointer, pointerDown, prevPointerDown, frame } game_ =
    case game_ of
        Running game ->
            case game.state of
                Aiming mbDragStartAt ->
                    let
                        newState =
                            case mbDragStartAt of
                                Nothing ->
                                    if
                                        pointerDown
                                            && not prevPointerDown
                                            && isPointInRectRI gc.ri pointer
                                    then
                                        initAimingState pointer

                                    else
                                        game.state

                                Just dragStartAt ->
                                    if not pointerDown then
                                        case validAimAngleTowards dragStartAt pointer of
                                            Nothing ->
                                                Aiming Nothing

                                            Just angle ->
                                                Simulating
                                                    (initSim
                                                        (initEmitter frame
                                                            (initBall game.ballPosition angle)
                                                            game.ballCount
                                                        )
                                                    )

                                    else
                                        game.state
                    in
                    ( { game | state = newState } |> stepGameTargets_ |> Running, Cmd.none )

                Simulating sim ->
                    case ballPositionOnSimEnd frame sim of
                        Just ballPosition ->
                            updateGameOnSimEnd frame ballPosition game

                        Nothing ->
                            stepSim frame game sim
                                |> mapFst (stepGameTargets_ >> Running)

        _ ->
            ( game_, Cmd.none )


updateGameOnSimEnd : Float -> Vec -> Game -> ( Game_, Cmd msg )
updateGameOnSimEnd now ballPosition game =
    let
        ( newTargets, newSeed ) =
            initTopRowTargets (game.turn + 1) game.seed
                |> mapFst ((++) (List.map moveTargetDown game.targets))

        ( constructor, newTurn, cmd ) =
            if canTargetsSafelyMoveDown game.targets then
                ( Running, game.turn + 1, Cmd.none )

            else
                ( Over, game.turn, playSound "over" )
    in
    ( constructor
        { game
            | transit = initTransit now
            , ballPosition = ballPosition
            , state = Aiming Nothing
            , turn = newTurn
            , targets = newTargets
            , seed = newSeed
        }
    , cmd
    )


stepGameTargets_ : Game -> Game
stepGameTargets_ game =
    let
        stepTargets : List Target -> List Target
        stepTargets =
            let
                stepTarget t =
                    case t.kind of
                        SolidTarget _ ->
                            t

                        BonusBallTarget c ->
                            { t | kind = BonusBallTarget (inc c) }

                        StarTarget c ->
                            { t | kind = StarTarget (inc c) }
            in
            List.map stepTarget
    in
    { game | targets = stepTargets game.targets }


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

        newStars =
            game.stars + acc.starsCollected
    in
    ( { game
        | state =
            Simulating
                { emitter = newEmitter
                , balls = Maybe.cons emittedBall newBalls.moved
                , floorBalls = addFloorBalls frame newBalls.floored sim.floorBalls
                , killSoundIdx = newKillSoundIdx
                , killAnims = addKillAnims frame acc.solidTargetsKilled sim.killAnims
                }
        , ballCount = acc.bonusBallsCollected + game.ballCount
        , stars = newStars
        , targets = acc.targets
      }
    , Cmd.batch
        [ cmdIf (acc.solidTargetHits > 0) (playSound "hit")
        , cmdIf (acc.bonusBallsCollected >= 1) (playSound "bonus_hit")
        , cmdIf (acc.starsCollected >= 1) (playSound "bonus_hit")
        , cmdIf (newKillSoundIdx > sim.killSoundIdx) (playKillSound newKillSoundIdx)
        , cmdIf (emittedBall /= Nothing) (playSound "shoot")
        , cmdIf (newStars /= game.stars) (saveStars newStars)
        ]
    )


addKillAnims : Float -> List Vec -> List KillAnim -> List KillAnim
addKillAnims frame newKillPositions killAnims =
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


type alias BallUpdateAcc =
    { targets : List Target
    , bonusBallsCollected : Int
    , starsCollected : Int
    , solidTargetHits : Int
    , solidTargetsKilled : List Vec
    }


initBallUpdateAcc : List Target -> BallUpdateAcc
initBallUpdateAcc targets =
    { targets = targets
    , bonusBallsCollected = 0
    , starsCollected = 0
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
                |> vecAdd gc.gravity
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
                    if gc.bottomEdge == e then
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

                        BonusBallTarget _ ->
                            ( { acc
                                | targets = remove target acc.targets
                                , bonusBallsCollected = inc acc.bonusBallsCollected
                              }
                            , BallMoved newBall
                            )

                        StarTarget _ ->
                            ( { acc
                                | targets = remove target acc.targets
                                , starsCollected = inc acc.starsCollected
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

                        BonusBallTarget _ ->
                            False

                        StarTarget _ ->
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
            ( ballToCircle ball, velocity )

        c1 : List ( Collision, BallCollision )
        c1 =
            gc.edges
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

        BonusBallTarget _ ->
            gc.ballR

        StarTarget _ ->
            gc.ballR
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (always OnTick)
            -- for monitors where refresh rate is >60 i.e. 144
            -- since we are not using `delta` for updates.
            -- also high refresh rate can lead to many updates
            -- perhaps, causing high load, in addition to its
            -- inherent implementation complexity.
            |> always (Time.every (1000 / 60) (always OnTick))
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
view (Model env page) =
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

        --, Icon.css
        , viewPage env page
        ]


viewPage : Env -> Page -> Html Msg
viewPage { vri, frame, pointer } page =
    Svg.svg (svgAttrs vri)
        [ --
          rect wc.ri [ fillP black ]
        , case page of
            StartPage _ ->
                group [ onClick StartGameClicked ]
                    [ rect (vec (gc.cellR * 2) (gc.cellR * 0.9)) [ fillP lightOrange ]
                    , words "START" [ fillP white, transform [ scale 3 ] ]
                    ]

            GamePage (Running g) ->
                group []
                    [ viewHeader g.turn
                    , viewFooter g.ballCount g.stars
                    , viewState frame pointer g.transit g.ballPosition g.turn g.targets g.state
                    ]

            GamePage (Paused g) ->
                group []
                    [ viewHeader g.turn
                    , viewFooter g.ballCount g.stars
                    , viewState frame pointer g.transit g.ballPosition g.turn g.targets g.state
                    , viewPauseOverlay
                    ]

            GamePage (Over g) ->
                group []
                    [ viewHeader g.turn
                    , viewFooter g.ballCount g.stars
                    , viewState frame pointer g.transit g.ballPosition g.turn g.targets g.state
                    , viewOverOverlay frame g.transit g.turn 991
                    ]
        ]


viewOverOverlay : Float -> Anim0 -> Int -> Int -> Svg Msg
viewOverOverlay frame transit score hiScore =
    let
        progress =
            transitProgress frame transit
    in
    group [ onClick HomeBtnClicked ]
        [ rect wc.ri [ fillP white, fade (progress |> lerp 0 0.9) ]
        , group
            [ fade progress
            , fillP black
            , T.fontWeight FontWeightBold
            ]
            [ group [ transform [ translateXY 0 -(gc.cellR * 4) ] ]
                [ words "SCORE" [ transform [ translateXY 0 -gc.cellR, scale 5 ] ]
                , words (String.fromInt score) [ transform [ translateXY 0 gc.cellR, scale 4 ] ]
                ]
            , group [ transform [ translateXY 0 gc.cellR ] ]
                [ words "BEST" [ transform [ translateXY 0 -gc.cellR, scale 5 ] ]
                , words (String.fromInt hiScore) [ transform [ translateXY 0 gc.cellR, scale 4 ] ]
                ]
            , btn ResumeGameClicked
                [ transform [ translateXY 0 (gc.cellR * 5), scale 1.5 ] ]
                [ viewIcon Icon.play ]
            ]
        ]


viewPauseOverlay : Svg Msg
viewPauseOverlay =
    let
        rightBtnX =
            gc.ri.x / 2.5
    in
    group []
        [ rect wc.ri [ fillP white, fade 0.8 ]
        , rect (vec (gc.ri.x * 0.7) (gc.cellR * 1.6)) [ fillP black, Px.rx gc.cellR ]
        , group [ fillP white ]
            [ btn RestartGameClicked
                [ transform [ translateXY -rightBtnX 0 ] ]
                [ viewRestartIcon ]
            , btn ResumeGameClicked
                [ transform [ translateXY 0 0 ] ]
                [ viewIcon Icon.play ]
            , btn HomeBtnClicked
                [ transform [ translateXY rightBtnX 0 ] ]
                [ viewIcon Icon.home ]
            ]
        , group
            [ fillP lightOrange
            ]
            [ words "PAUSED" [ transform [ translateXY 0 -50, scale 5 ] ]
            , words "Tap to Continue" [ transform [ translateXY 0 50, scale 3 ] ]
            ]
            |> hideView
        ]


viewHeader : Int -> Svg Msg
viewHeader turn =
    let
        rightBtnX =
            wc.header.ri.x - gc.cellR * 2
    in
    group [ transform [ translate wc.header.c ] ]
        [ rect wc.header.ri [ fillP darkCharcoal ]
        , group [ fillP white ]
            [ btn RestartGameClicked
                [ transform [ translateXY -rightBtnX 0 ] ]
                [ viewRestartIcon ]
            , words (String.fromInt turn)
                [ transform [ scale 4 ], T.fontWeight FontWeightBold ]
            , btn PauseGameClicked
                [ transform [ translateXY rightBtnX 0 ] ]
                [ viewIcon Icon.pause ]
            ]
        ]


btn msg aa =
    group
        (S.pointerEvents "bounding-box"
            :: S.cursor "pointer"
            :: alwaysPreventDefaultOn "click" (JD.succeed msg)
            :: aa
        )


viewRestartIcon : Svg msg
viewRestartIcon =
    viewRotatedIcon -45 Icon.redo


viewIcon : Icon -> Svg msg
viewIcon icon =
    viewRotatedIcon 0 icon


viewRotatedIcon : Float -> Icon -> Svg msg
viewRotatedIcon deg icon =
    Svg.path
        (transform
            [ rotate deg
            , scale ((gc.cellR / 512) * 1.1)
            , translateXY -(toFloat icon.width / 2) -(toFloat icon.height / 2)
            ]
            :: List.map S.d icon.paths
        )
        []


lightOrange : Color
lightOrange =
    hsl 0.07 0.8 0.5


viewFooter : Int -> Int -> Svg msg
viewFooter ballCount stars =
    let
        off =
            gc.cellR * 0.8

        off2 =
            off * 3
    in
    group [ transform [ translate wc.footer.c ] ]
        [ rect wc.footer.ri [ fillP lightOrange ]
        , group [ transform [ translateXY -off2 0 ] ]
            [ circle gc.ballR
                [ fillP white
                , transform [ translateXY -off 0 ]
                ]
            , words (String.fromInt ballCount)
                [ fillP white
                , transform [ translateXY off 0, scale 4 ]
                , T.fontWeight FontWeightBold
                ]
            ]
        , group [ transform [ translateXY off2 0 ] ]
            [ group
                [ fillP white
                , transform [ translateXY -off 0 ]
                ]
                [ starSvg ]
            , words (String.fromInt stars)
                [ fillP white
                , transform [ translateXY off 0, scale 4 ]
                , T.fontWeight FontWeightBold
                ]
            ]
        ]


viewState : Float -> Vec -> Anim0 -> Vec -> Int -> List Target -> State -> Svg Msg
viewState now pointer transit ballPosition turn targets state =
    case state of
        Aiming mbDragStartAt ->
            group []
                [ viewTargetsWithAnim now transit targets
                , viewBall ballPosition
                , if turn == 1 && isAnimDone now transit then
                    viewTutorial 0 now

                  else
                    noView
                , case
                    mbDragStartAt
                        |> Maybe.andThen
                            (\dragStartAt ->
                                validAimAngleTowards dragStartAt pointer
                                    |> Maybe.map (pair dragStartAt)
                            )
                  of
                    Just ( dragStartAt, angle ) ->
                        group []
                            [ viewTravelPath now (ballTravelPath targets ballPosition angle)
                            , viewDebugPoints [ pointer, dragStartAt ]
                            ]

                    Nothing ->
                        noView
                ]

        Simulating sim ->
            group []
                [ viewTargets targets
                , viewBalls (Maybe.cons (nextEmitterBall sim.emitter) sim.balls)
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
                , handSvg
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


starSvg : Svg msg
starSvg =
    Svg.path
        (S.d
            """m36.536 32.822c12.699-31.856 13.169-31.734 26.355-1.0603 34.15 2.198 39.384 9.6218
6.5131 23.932 19.821 46.048-0.79153 28.382-19.842 13.178-22.357 25.013-29.54
22.639-18.782-12.117-7.2813-8.5014-45.095-18.742 5.7558-23.932z"""
            :: [ transform [ scale 0.7, translateXY -51.2 -48 ] ]
        )
        []


handSvg : List (Svg.Attribute msg) -> Svg msg
handSvg aa =
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


viewBoxFromRI : { a | x : Float, y : Float } -> Attribute b
viewBoxFromRI ri =
    T.viewBox -ri.x -ri.y (ri.x * 2) (ri.y * 2)


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


maxPathLen : Float
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


viewTargetsWithAnim : Float -> Anim0 -> List Target -> Svg msg
viewTargetsWithAnim now anim targets =
    let
        progress =
            clampedAnimProgress now anim

        dy =
            (1 - progress) * -(gc.cri.y * 2)
    in
    group [ transform [ translateXY 0 dy ] ]
        (List.map viewTarget targets)


viewTargets : List Target -> Svg msg
viewTargets =
    viewTargetsWithAnimProgress 1


viewTargetsWithAnimProgress : Float -> List Target -> Svg msg
viewTargetsWithAnimProgress progress targets =
    let
        dy =
            (1 - progress) * -(gc.cri.y * 2)
    in
    group [ transform [ translateXY 0 dy ] ] (List.map viewTarget targets)


viewTarget : Target -> Svg msg
viewTarget target =
    let
        position =
            target.position
    in
    case target.kind of
        SolidTarget hp ->
            viewSolidTarget position hp

        BonusBallTarget clock ->
            viewBonusBall (bonusAnimPosition clock position)

        StarTarget clock ->
            viewStar (bonusAnimPosition clock position)


bonusAnimPosition : Float -> Vec -> Vec
bonusAnimPosition now position =
    vecAdd position (bonusAnimOffset now position)


bonusAnimOffset : Float -> Vec -> Vec
bonusAnimOffset now position =
    let
        staggeredNow =
            now + vecLenSq position

        ( ndx, ndy ) =
            ( wave 80 staggeredNow |> normToNegNorm, zigZag 90 staggeredNow |> normToNegNorm )
    in
    vec ndx ndy
        |> vecScale (gc.ballR * 0.3)


viewSolidTarget : Vec -> Int -> Svg msg
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
viewBalls balls =
    group [] (List.map (.position >> viewBall) balls)


viewStar : Vec -> Svg msg
viewStar p =
    group [ fillH bonusHue, transform [ translate p ] ] [ starSvg ]


viewBonusBall : Vec -> Svg msg
viewBonusBall p =
    let
        radius =
            gc.bonusR

        strokeW =
            radius * 0.4

        innerRadius =
            radius - strokeW / 2
    in
    group
        [ strokeH bonusHue
        , transform [ translate p ]
        , Px.strokeWidth strokeW
        ]
        [ Svg.circle [ Px.r innerRadius ] []
        ]


viewBall : Vec -> Svg msg
viewBall p =
    let
        radius =
            gc.ballR

        strokeW =
            radius * 0.4

        innerRadius =
            radius - strokeW / 2
    in
    group
        [ strokeP lightOrange
        , transform [ translate p ]
        , Px.strokeWidth strokeW
        ]
        [ Svg.circle [ Px.r innerRadius ] []
        ]


polyline : List Vec -> List (Attribute msg) -> Svg msg
polyline pts aa =
    Svg.polyline (points pts :: aa) []



--noinspection ElmUnusedSymbol


polySeg : ( Vec, Vec ) -> List (Attribute msg) -> Svg msg
polySeg ( a, b ) =
    polyline [ a, b ]


circle : Float -> List (Attribute msg) -> Svg msg
circle r aa =
    Svg.circle (Px.r r :: aa) []


rect : Vec -> List (Attribute msg) -> Svg msg
rect ri =
    vecApply rectWH (vecScale 2 ri)


rectWH : Float -> Float -> List (Attribute msg) -> Svg msg
rectWH w h aa =
    Svg.rect
        (Px.x (-w / 2)
            :: Px.y (-h / 2)
            :: Px.width w
            :: Px.height h
            :: aa
        )
        []


words : String -> List (Attribute msg) -> Svg msg
words txt aa =
    Svg.text_
        (T.alignmentBaseline AlignmentCentral
            :: T.textAnchor AnchorMiddle
            :: aa
        )
        [ Svg.text txt ]


points : List Vec -> Attribute msg
points =
    List.map vecToTuple >> T.points


fromHue : Float -> Color
fromHue hue =
    hsl hue 0.8 0.6


paintHue : Float -> Paint
paintHue =
    fromHue >> Paint


strokeH : Float -> Attribute msg
strokeH =
    paintHue >> T.stroke


fillH : Float -> Attribute msg
fillH =
    paintHue >> T.fill


group : List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
group =
    Svg.g



--noinspection ElmUnusedSymbol


fade : Float -> Attribute msg
fade o =
    T.opacity (Opacity o)



-- ANIM


type Anim a
    = Anim { start : Float, duration : Float, data : a }


transitionDuration : Float
transitionDuration =
    60 / 4


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


initTransit : Float -> Anim0
initTransit now =
    initAnim0 now transitionDuration


transitProgress : Float -> Anim a -> Float
transitProgress =
    clampedAnimProgress
