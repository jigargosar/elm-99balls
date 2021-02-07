module Main exposing (main)

import Browser
import Color
import Html exposing (Html)
import Random exposing (Generator)
import Random.Extra as Random
import Svg exposing (Svg)
import Svg.Attributes as S
import Time
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types exposing (Paint(..), Transform(..))
import Util exposing (..)


sw =
    400


sh =
    400


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
    Random.list 10 randomBall


randomBall : Generator Ball
randomBall =
    Random.map5 Ball
        (Random.map2 vec (Random.float (-sw / 2) (sw / 2)) (Random.float (-sh / 2) (sh / 2)))
        (Random.float 0 (turns 1))
        (Random.float 1 2)
        (Random.float 0 1)
        (Random.float 10 16)


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
    , normal = vecNormalFromTo from to
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
        ri =
            vec sw sh |> vecScale 0.5

        ( hw, hh ) =
            vecToTuple ri

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
            vecFromRTheta ball.speed ball.angle
    in
    { ball | position = vecAdd ball.position velocity }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1000 / 50) (always OnTick)
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
    Svg.polyline [ T.points (edgePoints2 edge), S.stroke "red", Px.strokeWidth 5 ] []


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
    Svg.circle
        [ Px.r ball.radius
        , T.fill (Paint (Color.hsl ball.hue 0.7 0.6))
        , T.transform [ vecApply Translate ball.position ]
        ]
        []


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
