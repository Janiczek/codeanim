module Time exposing
    ( frameToMs
    , ms
    , msToFrame
    , percentage
    )

import Config exposing (fpsF)


msToFrame : Float -> Float
msToFrame ms_ =
    ms_ * fpsF / 1000


ms : Int -> Int
ms ms_ =
    ceiling <| msToFrame (toFloat ms_)


frameToMs : Int -> Float
frameToMs frame =
    toFloat frame * 1000 / fpsF


percentage : Int -> Int -> Float
percentage a b =
    toFloat a / toFloat b
