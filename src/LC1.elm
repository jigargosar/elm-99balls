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
    { x : Int, y : Int }


chart : Html.Html msg
chart =
    LineChart.view1
        (.x >> toFloat)
        (.y >> toFloat)
        --[ Point 1 2, Point 5 5, Point 10 1 ]
        (Random.step rndPoints (Random.initialSeed 0)
            |> Tuple.first
            |> List.sortBy .x
        )


rndPoints =
    Random.list 50000 (rndBell 3 -10 10 |> rnd1 round)
        |> Random.map
            (List.Extra.gatherEquals
                >> List.map
                    (\( x, xs ) ->
                        Point x (List.length xs)
                    )
            )
