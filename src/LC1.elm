module LC1 exposing (..)

import Html exposing (div)
import LineChart
import LineChart.Colors
import LineChart.Dots
import List.Extra
import Random exposing (Generator)
import Random.Float
import Util exposing (..)


main : Html.Html msg
main =
    div [] [ bellChart ]


type alias Point =
    { x : Int, y : Int }


type alias BellPts =
    { b2 : List Point
    , b3 : List Point
    , b4 : List Point
    , b5 : List Point
    , normal : List Point
    }


bellChart : Html.Html msg
bellChart =
    let
        ( bellPts, _ ) =
            Random.step rndBellPts (Random.initialSeed 0)
    in
    LineChart.view
        (.x >> toFloat)
        (.y >> toFloat)
        [ LineChart.line LineChart.Colors.blue LineChart.Dots.none "Bell3" bellPts.b3
        , LineChart.line LineChart.Colors.pink LineChart.Dots.none "Bell2" bellPts.b2
        , LineChart.line LineChart.Colors.gold LineChart.Dots.none "Bell4" bellPts.b4
        , LineChart.line LineChart.Colors.black LineChart.Dots.none "Bell5" bellPts.b5

        --, LineChart.line LineChart.Colors.cyan LineChart.Dots.none "Normal" bellPts.normal
        ]


rndBellPts : Generator BellPts
rndBellPts =
    Random.map5 BellPts
        (rndPointsN 2)
        (rndPointsN 3)
        (rndPointsN 4)
        (rndPointsN 5)
        (Random.list 50000 (Random.Float.normal 0 (20 / 4) |> rnd1 round)
            |> Random.map
                (List.Extra.gatherEquals
                    >> List.map
                        (\( x, xs ) ->
                            Point x (List.length xs)
                        )
                    >> List.sortBy .x
                )
        )


rndPointsN : Int -> Generator (List Point)
rndPointsN n =
    Random.list 50000 (rndBellMO n 0 20 |> rnd1 round)
        |> Random.map
            (List.Extra.gatherEquals
                >> List.map
                    (\( x, xs ) ->
                        Point x (List.length xs)
                    )
                >> List.sortBy .x
            )
