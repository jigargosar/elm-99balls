module LC1 exposing (..)

import Html exposing (div)
import LineChart
import LineChart.Colors
import LineChart.Dots
import List.Extra
import Random exposing (Generator)
import Random.Float


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
    }


type alias NormalPts =
    { boxMuller : List Point
    , by4 : List Point
    }


type alias Data =
    { bell : BellPts
    , normal : NormalPts
    }


bellChart : Html.Html msg
bellChart =
    let
        ( { bell, normal }, _ ) =
            Random.step rndData (Random.initialSeed 0)
    in
    LineChart.view
        (.x >> toFloat)
        (.y >> toFloat)
        [ --
          --LineChart.line LineChart.Colors.pink LineChart.Dots.none "Bell2" bell.b2
          LineChart.line LineChart.Colors.blue LineChart.Dots.none "Bell3" bell.b3
        , LineChart.line LineChart.Colors.gold LineChart.Dots.none "Bell4" bell.b4
        , LineChart.line LineChart.Colors.black LineChart.Dots.none "Bell5" bell.b5

        --, LineChart.line LineChart.Colors.cyan LineChart.Dots.none "Normal" normal.default
        , LineChart.line LineChart.Colors.gray LineChart.Dots.none "NormalBy4" normal.by4
        ]


rndData : Generator Data
rndData =
    Random.map2 Data
        (Random.map4 BellPts
            (rndPointsBy (rndBellMO 2 0 20))
            (rndPointsBy (rndBellMO 3 0 20))
            (rndPointsBy (rndBellMO 4 0 20))
            (rndPointsBy (rndBellMO 5 0 20))
        )
        (Random.map2 NormalPts
            (rndPointsBy (Random.Float.normal 0 20))
            (rndPointsBy (Random.Float.normal 0 (20 / 4)))
        )


rndPointsBy : Generator Float -> Generator (List Point)
rndPointsBy gen =
    Random.list 30000 (gen |> Random.map round)
        |> Random.map
            (List.Extra.gatherEquals
                >> List.map
                    (\( x, xs ) ->
                        Point x (List.length xs)
                    )
                >> List.sortBy .x
            )


rndBellMO : Int -> Float -> Float -> Generator Float
rndBellMO n mid offset =
    rndBell n (mid - offset) (mid + offset)


rndBell : Int -> Float -> Float -> Generator Float
rndBell n lo hi =
    Random.list n (Random.float lo hi)
        |> Random.map (List.sum >> (\total -> total / toFloat n))
