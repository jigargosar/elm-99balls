module Main exposing (main)

import Svg
import TypedSvg.Attributes as T
import TypedSvg.Attributes.InPx as Px


main =
    let
        ( w, h ) =
            ( 400, 400 )
    in
    Svg.svg
        [ T.viewBox (-w / 2) (-h / 2) w h
        , Px.width w
        , Px.height h
        ]
        [ rect w h [] [] ]


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
