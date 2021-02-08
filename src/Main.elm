module Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import List.Extra as List
import Random exposing (Generator)
import Random.Extra as Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes as S
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types exposing (Paint(..), Transform(..))
import Util exposing (..)


sw =
    400


sh =
    400


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
    Random.list 150 randomBall


randomBall : Generator Ball
randomBall =
    Random.map5 Ball
        --(randomVecInRadii sri)
        (Random.choices
            (randomVec (sri.x - 10) sri.x -sri.y sri.y)
            [ randomVec -sri.x (-sri.x + 10) -sri.y sri.y
            , rv (rfO sri.x) (rfSO -sri.y 10)
            , rv (rfO sri.x) (rf sri.y (sri.y - 10))
            ]
        )
        (Random.float 0 (turns 1))
        (Random.float 1 2)
        (Random.float 0 1)
        (Random.float 10 16)


rv a b =
    Random.map2 vec a b


rf =
    Random.float


rfO : Float -> Generator Float
rfO o =
    rfMO 0 o


rfMO : Float -> Float -> Generator Float
rfMO m o =
    rf (m - o) (m + o)


rfSO s o =
    rf s (s + o)


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


type Msg
    = OnTick


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        initialSeed =
            Random.initialSeed 0

        ( balls, _ ) =
            Random.step randomBalls initialSeed
    in
    ( { balls = balls }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnTick ->
            ( { model | balls = List.map updateBall model.balls }, Cmd.none )


updateBall : Ball -> Ball
updateBall ball =
    let
        velocity =
            computeNewBallVelocity ball

        angle =
            vecToPolar velocity |> Tuple.second
    in
    { ball
        | position = vecAdd ball.position velocity
        , angle = angle
    }



--updateBall : Ball -> Ball
--updateBall ball =
--    let
--        velocity =
--            vecFromRTheta ball.speed ball.angle
--
--        collision =
--            List.any (ballEdgeCollision ball) edges
--    in
--    if not collision then
--        { ball | position = vecAdd ball.position velocity }
--
--    else
--        ball


computeNewBallVelocity : Ball -> Vec
computeNewBallVelocity ball =
    let
        velocity =
            vecFromRTheta ball.speed ball.angle
    in
    case List.find (ballEdgeCollision ball) edges of
        Nothing ->
            velocity

        Just edge ->
            let
                n =
                    edge.normal
            in
            vecSub velocity (vecScale 2 (vecScale (vecDotProduct n velocity) n))


ballEdgeCollision : Ball -> Edge -> Bool
ballEdgeCollision ball edge =
    let
        velocity =
            vecFromRTheta ball.speed ball.angle

        ballPosition =
            vecAdd ball.position velocity

        n =
            edge.normal

        a =
            vecFromTo edge.from ballPosition

        c =
            vecDotProduct a n
    in
    c < ball.radius


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (always OnTick)
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
        [ rect sw sh [] [ S.stroke "black" ]
        , Svg.g [] (List.map viewBall model.balls)
        , Svg.g [] (List.map viewEdge edges)
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


viewBall : Ball -> Svg Msg
viewBall ball =
    let
        p1 =
            ball.position

        p2 =
            vecFromRTheta ball.radius ball.angle
                |> vecAdd p1
    in
    Svg.g []
        [ Svg.circle
            [ Px.r ball.radius
            , T.stroke (Paint (Color.hsl ball.hue 0.7 0.6))
            , Px.strokeWidth 1
            , T.transform [ vecApply Translate ball.position ]
            ]
            []
        , Svg.polyline
            [ T.points (List.map vecToTuple [ p1, p2 ])
            , T.stroke (Paint (Color.hsl ball.hue 0.7 0.6))
            , Px.strokeWidth 2
            ]
            []
        ]


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
