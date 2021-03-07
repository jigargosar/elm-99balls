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
    { n2 : List Point
    , n3 : List Point
    , n4 : List Point
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
        [ LineChart.dash LineChart.Colors.pink LineChart.Dots.none "N2" [ 4, 2 ] bellPts.n2
        , LineChart.line LineChart.Colors.blue LineChart.Dots.none "N3" bellPts.n3
        , LineChart.line LineChart.Colors.gold LineChart.Dots.none "N4" bellPts.n4
        ]


rndBellPts : Generator BellPts
rndBellPts =
    Random.map3 BellPts
        (rndPointsN 2)
        (rndPointsN 3)
        (rndPointsN 4)


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
