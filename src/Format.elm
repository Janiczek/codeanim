module Format exposing (framesAsSeconds)

import Time


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
