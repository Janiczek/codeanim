module Time exposing
    ( frameToMs
    , msToFrame
    , percentage
    )

import Config exposing (fpsF)


msToFrame : Float -> Float
msToFrame ms =
    ms * fpsF / 1000


frameToMs : Int -> Float
frameToMs frame =
    toFloat frame * 1000 / fpsF


percentage : Int -> Int -> Float
percentage a b =
    toFloat a / toFloat b
