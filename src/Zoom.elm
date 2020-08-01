module Zoom exposing (msToPx)


globalMultiplier : Float
globalMultiplier =
    1 / 20


msToPx : { ms : Int, zoom : Int } -> Int
msToPx { ms, zoom } =
    let
        zoomMultiplier : Float
        zoomMultiplier =
            1 + toFloat zoom / 10
    in
    round <| toFloat ms * globalMultiplier * zoomMultiplier
