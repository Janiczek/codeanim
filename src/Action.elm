module Action exposing
    ( Action(..)
    , duration
    )


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
