module Format exposing
    ( framesAsSeconds
    , percentage
    )

import Time


{-| p = 0..1
-}
percentage : Float -> String
percentage p =
    String.fromInt (round (p * 100)) ++ "%"


framesAsSeconds : Int -> String
framesAsSeconds frames =
    let
        ms : Int
        ms =
            Time.frameToMs frames
                |> ceiling

        raw : String
        raw =
            String.fromFloat (toFloat (ms // 100) / 10)

        processed : String
        processed =
            if not (String.contains "." raw) then
                raw ++ ".0"

            else
                raw
    in
    processed ++ "s"
