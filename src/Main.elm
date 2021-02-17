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

  - allow multiple input handling mode for quick switch during testing

  - test input handling on phone/touch device.

  - Concern: Input handling is much better, but there is room for improvement.
      - carpel tunnel pain: try tap rather than drag, perhaps only for mouse

  - add nice `ting` sound on target ball collision

  - Understand exactly how collision computes time `t` and document or return clarifying types.

  - Understand what happens, currently, when objects are already colliding,
    i.e. when objects already intersecting, what is effect on collision response.

  - Invalid states, that are representable.
      - ball outside screen edges / sim never ending.
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
    , balls : List Ball
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
    Target (toWorld gp) gc.targetR hp


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


initBall : Float -> Ball
initBall angle =
    { position = vec 0 (gc.ri.y - gc.ballR)
    , angle = angle
    , hue = 0.15
    , radius = gc.ballR
    , speed = gc.ballR * 0.9
    }


randomBall : Generator Ball
randomBall =
    rnd1 initBall (rndF (turns 0.6) (turns 0.9))


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
      , vri = gc.ri
      , seed = seed
      }
        |> addNewTargetRow
        |> addNewTargetRow
        |> addNewTargetRow
    , Dom.getViewport |> Task.perform GotDomViewPort
    )


randomLevel : Generator ( List Ball, List Target )
randomLevel =
    rndPair
        (rndList 15 randomBall)
        randomTargets


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
                { model
                    | state =
                        WaitingForInput
                }

            else
                model

        WaitingForInput ->
            if model.pointerDown then
                { model | state = DraggingPointer model.pointer }

            else
                model

        DraggingPointer startPointer ->
            if model.pointerDown then
                model

            else
                case validInputAngleV2 model startPointer of
                    Nothing ->
                        { model | state = WaitingForInput }

                    Just angle ->
                        { model | state = Sim }
                            |> startSimAtAngle angle

        Sim ->
            -- check for turn over
            if (model.maybeEmitter == Nothing) && (model.balls == []) then
                -- check for game over
                if canTargetsSafelyMoveDown model.targets then
                    { model | state = TargetsEntering model.frame }
                        |> addNewTargetRow

                else
                    -- game over : for now re-simulate current turn.
                    { model | state = TargetsEntering model.frame }

            else
                model
                    |> moveBallsAndHandleCollision
                    |> emitBalls


firstFloorBall : Model -> Maybe Ball
firstFloorBall model =
    model.floorBalls |> List.head


firstFloorBallPosition : Model -> Maybe Vec
firstFloorBallPosition model =
    firstFloorBall model |> Maybe.map .position


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
    let
        ball =
            initBall angle
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
                , viewBalls model.balls
                , viewEdges
                , case model.maybeEmitter of
                    Just emitter ->
                        viewBalls [ emitter.next ]

                    Nothing ->
                        viewNone
                , case model.state of
                    DraggingPointer startPointer ->
                        case validInputAngleV2 model startPointer of
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
                , viewDebugPointer model.pointer
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
    let
        ball =
            initBall angle
    in
    ballTravelPathHelp model ball 0 [ ball.position ]


maxPathLen =
    gc.ri.y * 2.2


ballTravelPathHelp : Model -> Ball -> Float -> List Vec -> List Vec
ballTravelPathHelp model ball pathLen path =
    let
        ( bc, newBall ) =
            updateBallHelp model.targets ball

        newPathLenSq =
            vecLenFromTo ball.position newBall.position + pathLen
    in
    if newPathLenSq > maxPathLen || bc == BallHitBottomEdge then
        path

    else
        ballTravelPathHelp model newBall newPathLenSq (newBall.position :: path)


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
                    , T.fontFamily [ "monospace" ]
                    , Px.fontSize (target.radius * 1.5)

                    --, T.fontWeight FontWeightBold
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
                    radius * 0.3

                innerRadius =
                    radius - strokeW / 2
            in
            group
                [ strokeH hue
                , transform [ translate p ]
                , Px.strokeWidth strokeW
                ]
                [ Svg.circle [ Px.r innerRadius ] []
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
