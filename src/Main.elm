module Main exposing (main)

import Basics.Extra exposing (uncurry)
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
    { x : Float
    , y : Float
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
    Random.map6 Ball
        (Random.float (-sw / 2) (sw / 2))
        (Random.float (-sh / 2) (sh / 2))
        (Random.float 0 (turns 1))
        (Random.float 1 2)
        (Random.float 0 1)
        (Random.float 10 16)


type alias Edge =
    { xa : Float, ya : Float, xb : Float, yb : Float }


edgePoints : Edge -> List ( Float, Float )
edgePoints edge =
    [ ( edge.xa, edge.ya ), ( edge.xb, edge.yb ) ]


edges : List Edge
edges =
    let
        ( hw, hh ) =
            ( sw / 2, sh / 2 )
    in
    [ -- TopLeft -> TopRight
      Edge -hw -hh hw -hh
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
        ( dx, dy ) =
            fromPolar ( ball.speed, ball.angle )

        ( nx, ny ) =
            ( ball.x, ball.y )
                |> map2 (+) ( dx, dy )
    in
    { ball | x = nx, y = ny }


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


dotProduct : ( number, number ) -> ( number, number ) -> number
dotProduct a b =
    map2 (*) a b |> uncurry (+)


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
        ]


viewEdge : Edge -> Svg Msg
viewEdge edge =
    Svg.polyline [ T.points (edgePoints edge), S.stroke "black", Px.strokeWidth 2 ] []


viewBall : Ball -> Svg Msg
viewBall ball =
    Svg.circle
        [ Px.r ball.radius
        , T.fill (Paint (Color.hsl ball.hue 0.7 0.6))
        , T.transform [ Translate ball.x ball.y ]
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
