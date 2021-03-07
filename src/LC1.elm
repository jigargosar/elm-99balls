module LC1 exposing (..)

import Html
import LineChart
import List.Extra
import Random exposing (Generator)
import Random.Float
import Util exposing (..)


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


bellN : Int -> Generator Float
bellN n =
    rndList n (rndF -1 1)
        |> rnd1 (List.sum >> (\total -> total / toFloat n))


rndPoints =
    Random.list 50000
        (Random.float -1 1
            |> always Random.Float.standardNormal
            |> always (bellN 4)
            |> always (bellN 3)
        )
        |> Random.map
            (identity
                >> List.Extra.gatherEqualsBy (\x -> x * 20 |> round)
                >> List.sortBy Tuple.first
                >> List.map
                    (\( x, xs ) ->
                        Point x (List.length xs |> toFloat)
                    )
            )
