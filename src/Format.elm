module Format exposing (msAsSeconds)


msAsSeconds : Int -> String
msAsSeconds ms =
    let
        raw =
            String.fromFloat (toFloat (ms // 100) / 10)

        processed =
            if not (String.contains "." raw) then
                raw ++ ".0"

            else
                raw
    in
    processed ++ "s"
