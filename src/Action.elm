module Action exposing
    ( Action(..)
    , duration
    , tooltip
    )

import Format


type Action
    = TypeText
        { text : String
        , duration : Int
        , position : Maybe Int -- Nothing = at the end
        }
    | Wait { duration : Int }
    | FadeOut { duration : Int }


duration : Action -> Int
duration action =
    case action of
        TypeText r ->
            r.duration

        Wait r ->
            r.duration

        FadeOut r ->
            r.duration


tooltip : Action -> String
tooltip action =
    case action of
        TypeText { text } ->
            "Type: "
                ++ (if String.length text > 10 then
                        String.left 10 text ++ "..."

                    else
                        text
                   )

        Wait r ->
            "Wait " ++ Format.msAsSeconds r.duration

        FadeOut r ->
            "Fade out in " ++ Format.msAsSeconds r.duration
