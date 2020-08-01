module Zoom exposing (framesToPx, msToPx)

import Time


globalMultiplier : Float
globalMultiplier =
    1 / 20


framesToPx : { frames : Int, zoom : Int } -> Int
framesToPx { frames, zoom } =
    msToPx
        { ms = ceiling (Time.frameToMs frames)
        , zoom = zoom
        }


msToPx : { ms : Int, zoom : Int } -> Int
msToPx { ms, zoom } =
    let
        zoomMultiplier : Float
        zoomMultiplier =
            1 + toFloat zoom / 10
    in
    round <| toFloat ms * globalMultiplier * zoomMultiplier
