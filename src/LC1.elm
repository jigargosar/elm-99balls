module LC1 exposing (..)

import Html
import LineChart
import List.Extra
import Random
import Random.Float


main : Html.Html msg
main =
    chart


type alias Point =
    { x : Float, y : Float }


chart : Html.Html msg
chart =
    LineChart.view1
        .x
        .y
        --[ Point 1 2, Point 5 5, Point 10 1 ]
        (Random.step rndPoints (Random.initialSeed 0) |> Tuple.first)


rndPoints =
    Random.list 1000 Random.Float.standardNormal
        |> Random.map
            (identity
                >> List.Extra.gatherEqualsBy (\x -> x * 10 |> round)
                >> List.sortBy Tuple.first
                >> List.map
                    (\( x, xs ) ->
                        Point x (List.length xs |> toFloat)
                    )
            )
