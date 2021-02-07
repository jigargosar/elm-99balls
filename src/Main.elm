module Main exposing (main)

import Browser
import Html exposing (Html)
import Random exposing (Generator)
import Svg
import Svg.Attributes as S
import Time
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px


w =
    400


h =
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
    , hue : Float
    , radius : Float
    }


randomBalls : Generator (List Ball)
randomBalls =
    Random.list 10 randomBall


randomBall : Generator Ball
randomBall =
    Random.map5 Ball
        (Random.float (-w / 2) (w / 2))
        (Random.float (-h / 2) (h / 2))
        (Random.float 0 (turns 1))
        (Random.float 0 1)
        (Random.float 10 16)


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
            ( { model | y = model.y + 2 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1000 / 50) (always OnTick)
        ]


view : Model -> Html Msg
view model =
    Svg.svg
        [ T.viewBox (-w / 2) (-h / 2) w h
        , Px.width w
        , Px.height h
        , S.fill "none"
        , S.stroke "none"
        ]
        [ rect w h [] [ S.stroke "black" ]
        , circle 20 [] [ S.fill "black" ]
        ]


circle r xf aa =
    Svg.circle
        (Px.r r
            :: T.transform xf
            :: aa
        )
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
