module Main exposing (main)

import Browser
import Html exposing (Html)
import Svg
import Svg.Attributes as S
import Time
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px


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
    {}


type Msg
    = OnTick


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnTick ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1000 / 50) (always OnTick)
        ]


view : Model -> Html Msg
view _ =
    let
        ( w, h ) =
            ( 400, 400 )
    in
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
