module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Color exposing (..)
import Html exposing (Html, div, node)
import Html.Attributes as A exposing (style)
import Html.Events as E
import Json.Decode as JD exposing (Decoder)
import List.Extra as List
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

  - goodies: extra ball

  - test input handling on phone/touch device.
      - Concern: angle flickering on touch release


# Later

  - Concern: Input handling is much better, but there is room for improvement.
      - carpel tunnel pain: try tap rather than drag, perhaps only for mouse

  - add nice `ting` sound on target ball collision

  - Understand exactly how collision computes time `t` and document or return clarifying types.

  - Understand what happens, currently, when objects are already colliding,
    i.e. when objects already intersecting, what is effect on collision response.

  - Invalid states, that are representable.
      - +1 ball outside screen edges / sim never ending.
          - Bug: ball outside world, invariant failed during mobile testing InputV1
      - total number of balls before and after simulation are different.

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
    { maybeEmitter : Maybe Emitter
    , floorBalls : List Ball
    , targets : List Target
    , state : State
    , pointerDown : Bool
    , pointer : Vec
    , prevPointer : Vec
    , frame : Float
    , vri : Vec
    , seed : Seed
    }


animDur : Float
animDur =
    60 / 4


type State
    = TargetsEntering Float
    | WaitingForInput
    | DraggingPointer Vec
    | Sim { balls : List Ball }


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



--randomTargets1 : Generator (List Target)
--randomTargets1 =
--    let
--        randomTargetPositions =
--            rnd2 List.drop (rndInt 1 3) (rndShuffle gc.topRowPS)
--    in
--    randomTargetPositions
--        |> rndAndThen (List.map randomSolidTarget >> rndCombine)


randomTargets : Generator (List Target)
randomTargets =
    List.map randomTarget gc.topRowPS
        |> rndCombine
        |> Random.map (List.filterMap identity)


randomTarget : ( Int, Int ) -> Generator (Maybe Target)
randomTarget gp =
    Random.frequency
        ( 25, Random.constant Nothing )
        [ ( 70, randomSolidTarget gp |> Random.map Just )
        , ( 5, Random.constant (Just (Target (toWorld gp) ExtraBallTarget)) )
        ]


randomSolidTarget : ( Int, Int ) -> Generator Target
randomSolidTarget gp =
    rnd1 (initSolidTarget gp) (rndInt 1 maxHP)


initSolidTarget : ( Int, Int ) -> Int -> Target
initSolidTarget gp hp =
    Target (toWorld gp) (SolidTarget hp)


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
                |> Maybe.map (.position >> fromWorld >> snd)
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


type alias Ball =
    { position : Vec
    , angle : Float
    , speed : Float
    , hue : Float
    , radius : Float
    }


initBallAtBottomCenter : Ball
initBallAtBottomCenter =
    initBall (vec 0 (gc.ri.y - gc.ballR)) (turns -0.25)


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
    boundingSegFromRadii gc.ri


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
    = GotDomViewPort Dom.Viewport
    | OnDomResize Int Int
    | OnTick Float
    | PointerDown Bool Vec
    | PointerMoved Vec


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        initialSeed =
            seedFrom 4

        ( targets, seed ) =
            rndStep ( randomTargets, initialSeed )

        initialBallCount =
            10
    in
    ( { maybeEmitter = Nothing
      , floorBalls = List.repeat initialBallCount initBallAtBottomCenter
      , targets = targets
      , pointerDown = False
      , pointer = vecZero
      , prevPointer = vecZero
      , state = TargetsEntering 0
      , frame = 0
      , vri = gc.ri
      , seed = seed
      }
        |> addNewTargetRow
        |> addNewTargetRow
        |> addNewTargetRow
    , Dom.getViewport |> Task.perform GotDomViewPort
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotDomViewPort { viewport } ->
            ( { model | vri = vec viewport.width viewport.height |> vecScale 0.5 }, Cmd.none )

        OnDomResize w h ->
            ( { model | vri = vec (toFloat w) (toFloat h) |> vecScale 0.5 }, Cmd.none )

        OnTick _ ->
            ( updateOnTick model
                |> convergeFloorBalls
                |> incFrame
                |> cachePointer
            , Cmd.none
            )

        PointerDown isDown pointer ->
            ( { model
                | pointerDown = isDown
                , pointer = pointer |> svgToWorld model
              }
            , Cmd.none
            )

        PointerMoved pointer ->
            ( { model
                | pointer = pointer |> svgToWorld model
              }
            , Cmd.none
            )


svgToWorld : Model -> Vec -> Vec
svgToWorld model svgCord =
    let
        svgRI =
            computeSvgRI model

        svgScale =
            gc.ri.x / svgRI.x
    in
    vecSub svgCord svgRI
        |> vecScale svgScale


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
                { model | state = WaitingForInput }

            else
                model

        WaitingForInput ->
            if model.pointerDown then
                { model | state = DraggingPointer model.pointer }

            else
                model

        DraggingPointer start ->
            if not model.pointerDown then
                case validInputAngle model start of
                    Nothing ->
                        { model | state = WaitingForInput }

                    Just angle ->
                        { model | state = Sim { balls = [] } }
                            |> startSimAtAngle angle

            else
                model

        Sim sim ->
            -- check for turn over
            if (model.maybeEmitter == Nothing) && (sim.balls == []) then
                -- check for game over
                if canTargetsSafelyMoveDown model.targets then
                    { model | state = TargetsEntering model.frame }
                        |> addNewTargetRow

                else
                    -- game over : for now re-simulate current turn.
                    { model | state = TargetsEntering model.frame }

            else
                moveBallsAndHandleCollision sim.balls model
                    |> (\( balls, nm ) ->
                            case model.maybeEmitter |> Maybe.andThen (emitBalls nm) of
                                Nothing ->
                                    { nm | state = Sim { balls = balls } }

                                Just ( ball, maybeEmitter ) ->
                                    { nm | state = Sim { balls = ball :: balls }, maybeEmitter = maybeEmitter }
                       )


firstFloorBall : Model -> Maybe Ball
firstFloorBall model =
    model.floorBalls |> List.head


firstFloorBallPosition : Model -> Maybe Vec
firstFloorBallPosition model =
    firstFloorBall model |> Maybe.map .position


type InputVersion
    = InputV1
    | InputV2


validInputAngle : Model -> Vec -> Maybe Float
validInputAngle =
    case InputV1 of
        InputV1 ->
            validInputAngleV1

        InputV2 ->
            validInputAngleV2


validInputAngleV1 : Model -> Vec -> Maybe Float
validInputAngleV1 model start =
    let
        current =
            model.pointer
    in
    if start.y < current.y then
        vecAngleFromTo current start
            |> clampInputAngle
            |> Just

    else
        Nothing


validInputAngleV2 : Model -> Vec -> Maybe Float
validInputAngleV2 model start =
    firstFloorBallPosition model
        |> Maybe.andThen
            (\ballCenter ->
                let
                    current =
                        model.pointer

                    angleOffset =
                        angleABC current start ballCenter
                            |> mul 0.1
                in
                if start.y < current.y then
                    Just
                        (vecAngleFromTo ballCenter start
                            |> add angleOffset
                            |> clampInputAngle
                        )

                else
                    Nothing
            )


clampInputAngle =
    clampMO (turns -0.25) (turns 0.24)


incFrame : Model -> Model
incFrame model =
    { model | frame = inc model.frame }


emitBalls : Model -> Emitter -> Maybe ( Ball, Maybe Emitter )
emitBalls model emitter =
    if model.frame - emitter.start > 10 then
        Just
            ( emitter.next
            , case emitter.rest of
                [] ->
                    Nothing

                n :: r ->
                    Just (Emitter model.frame n r)
            )

    else
        Nothing


mapFloorBalls : (Ball -> List Ball -> ( Ball, List Ball )) -> Model -> Maybe Model
mapFloorBalls fn model =
    List.unconsLast model.floorBalls
        |> Maybe.map
            (\( last, rest ) ->
                let
                    ( nLast, nRest ) =
                        fn last rest
                in
                { model | floorBalls = nRest ++ [ nLast ] }
            )


convergeFloorBalls : Model -> Model
convergeFloorBalls model =
    model
        |> mapFloorBalls
            (\last others ->
                ( last, List.map (convergeBallTowards last.position) others )
            )
        |> Maybe.withDefault model


convergeBallTowards : Vec -> Ball -> Ball
convergeBallTowards to ball =
    let
        p =
            vecFromTo ball.position to
                |> vecScale 0.1
                |> vecAdd ball.position
    in
    setBallPosition p ball


moveBallsAndHandleCollision : List Ball -> Model -> ( List Ball, Model )
moveBallsAndHandleCollision balls model =
    let
        ( { targets, extraBallsCollected }, ballUpdates ) =
            balls
                |> List.mapAccuml updateBall
                    { targets = model.targets
                    , extraBallsCollected = 0
                    }

        { floored, updated } =
            splitBallUpdates ballUpdates
    in
    ( updated
    , { model
        | targets = targets
        , floorBalls = floored ++ model.floorBalls
      }
    )


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
    case firstFloorBallPosition model of
        Nothing ->
            model

        Just ballPosition ->
            let
                ball =
                    initBall ballPosition angle
            in
            { model
                | floorBalls = []
                , maybeEmitter =
                    Just
                        (Emitter model.frame
                            ball
                            (List.repeat (List.length model.floorBalls - 1) ball)
                        )
            }


areFloorBallsSettled : Model -> Bool
areFloorBallsSettled model =
    model.floorBalls
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
    , extraBallsCollected : Int
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
                                    | targets = List.setIf (eq target) { target | kind = SolidTarget (hp - 1) } acc.targets
                                  }
                                , BallMoved newBall
                                )

                        ExtraBallTarget ->
                            ( { acc
                                | targets = reject (eq target) acc.targets
                                , extraBallsCollected = acc.extraBallsCollected + 1
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
    = BallEdgeCollision Edge
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
                        detectMovingCircleAndSegCollision mc (edgeToSeg e)
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


offsetDecoder : Decoder Vec
offsetDecoder =
    JD.map2 vec (JD.field "offsetX" JD.float) (JD.field "offsetY" JD.float)


computeSvgRI : Model -> Vec
computeSvgRI model =
    let
        ar =
            gc.aspectRatio

        vri =
            model.vri
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
    let
        svgRi =
            computeSvgRI model
    in
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "height" "100%"
        ]
        [ node "link" [ A.href "styles.css", A.rel "stylesheet" ] []
        , Svg.svg
            [ T.viewBox -gc.ri.x -gc.ri.y (gc.ri.x * 2) (gc.ri.y * 2)
            , Px.width (svgRi.x * 2)
            , Px.height (svgRi.y * 2)
            , S.fill "none"
            , S.stroke "none"
            , E.on "pointerdown" (offsetDecoder |> JD.map (PointerDown True))
            , E.on "pointerup" (offsetDecoder |> JD.map (PointerDown False))
            , E.on "pointermove" (offsetDecoder |> JD.map PointerMoved)
            , style "touch-action" "none"
            , style "user-select" "none"
            ]
            [ group []
                [ rect gc.ri [ fillP black ]
                , viewFloorBalls model.floorBalls
                , case model.state of
                    TargetsEntering start ->
                        viewTargets ((model.frame - start) / animDur |> clamp 0 1) model.targets

                    _ ->
                        viewTargets 1 model.targets
                , viewEdges
                , case model.maybeEmitter of
                    Just emitter ->
                        viewBalls [ emitter.next ]

                    Nothing ->
                        viewNone
                , case model.state of
                    Sim { balls } ->
                        viewBalls balls

                    DraggingPointer startPointer ->
                        case validInputAngle model startPointer of
                            Nothing ->
                                viewNone

                            Just angle ->
                                group []
                                    [ viewTravelPath model.frame
                                        (ballTravelPathAtAngle angle model)
                                    , circle 10 [ fillH 0.4, transform [ translate model.pointer ] ]
                                    , circle 10 [ fillH 0.4, transform [ translate startPointer ] ]
                                    ]

                    _ ->
                        viewNone
                , viewDebugPointer model.pointer |> always viewNone
                ]
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
        [ circle (gc.ballR * 0.5) [ fillH 0.4, transform [ translate pointer ] ]
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


ballTravelPathAtAngle : Float -> Model -> List Vec
ballTravelPathAtAngle angle model =
    case firstFloorBallPosition model of
        Nothing ->
            []

        Just ballPosition ->
            let
                ball =
                    initBall ballPosition angle
            in
            ballTravelPathHelp
                { targets = model.targets
                , extraBallsCollected = 0
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


viewTargets : Float -> List Target -> Svg msg
viewTargets progress targets =
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
                        |> vecScale (gc.ri.x * 0.1)

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


viewNone =
    Svg.text ""
