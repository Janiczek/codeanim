module Zoom exposing (frameToPx, pxToFrame)

import Time


globalMultiplier : Float
globalMultiplier =
    1 / 20


zoomMultiplier : Int -> Float
zoomMultiplier zoom =
    1 + toFloat zoom / 10


frameToPx : { frame : Int, zoom : Int } -> Int
frameToPx { frame, zoom } =
    floor <|
        Time.frameToMs frame
            * globalMultiplier
            * zoomMultiplier zoom


pxToFrame : { px : Int, zoom : Int } -> Int
pxToFrame { px, zoom } =
    ceiling <|
        Time.msToFrame <|
            toFloat px
                / globalMultiplier
                / zoomMultiplier zoom
