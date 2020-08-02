module Action exposing
    ( Action
    , FadeOutOptions
    , RawAction(..)
    , TypeTextOptions
    , WaitOptions
    , bgColor
    , durationFrames
    , label
    , process
    , tooltip
    )

import Element as E
import Format


bgColor : RawAction -> E.Color
bgColor action =
    case action of
        TypeText _ ->
            E.rgb255 0xCC 0xDD 0xD0

        Wait _ ->
            E.rgb255 0xDD 0xBB 0xB0

        FadeOut _ ->
            E.rgb255 0xBB 0xCC 0xDD

        BlankText ->
            E.rgb255 0xB0 0xB0 0xBB

        SetText _ ->
            E.rgb255 0xBC 0xCD 0xC0


type RawAction
    = TypeText TypeTextOptions
    | Wait WaitOptions
    | FadeOut FadeOutOptions
    | BlankText
    | SetText String


type alias Action =
    { raw : RawAction
    , startFrame : Int
    , endFrame : Int
    }


type alias TypeTextOptions =
    { text : String
    , durationFrames : Int
    , position : Maybe Int -- Nothing = at the end
    }


type alias WaitOptions =
    { durationFrames : Int }


type alias FadeOutOptions =
    { durationFrames : Int }


durationFrames : RawAction -> Int
durationFrames action =
    case action of
        TypeText r ->
            r.durationFrames

        Wait r ->
            r.durationFrames

        FadeOut r ->
            r.durationFrames

        BlankText ->
            {- TODO make this ms-based so that it takes the same amount of time
               with different FPS settings
            -}
            6

        SetText _ ->
            {- TODO make this ms-based so that it takes the same amount of time
               with different FPS settings
            -}
            6


snippet : String -> String
snippet text =
    if String.length text > 10 then
        String.left 10 text ++ "..."

    else
        text


label : RawAction -> String
label action =
    case action of
        TypeText _ ->
            "Type"

        Wait _ ->
            "Wait"

        FadeOut _ ->
            "FadeOut"

        BlankText ->
            "Blank"

        SetText _ ->
            "Set"


tooltip : RawAction -> String
tooltip action =
    case action of
        TypeText { text } ->
            "Type: "
                ++ snippet text

        Wait r ->
            "Wait " ++ Format.framesAsSeconds r.durationFrames

        FadeOut r ->
            "Fade out in " ++ Format.framesAsSeconds r.durationFrames

        BlankText ->
            "Blank text"

        SetText text ->
            "Set text: " ++ snippet text


process : List RawAction -> ( Int, List Action )
process actions =
    List.foldl
        (\action ( accFrames, accActions ) ->
            let
                nextActionStartFrame =
                    accFrames + durationFrames action
            in
            ( nextActionStartFrame
            , { raw = action
              , startFrame = accFrames
              , endFrame = nextActionStartFrame - 1
              }
                :: accActions
            )
        )
        ( 0, [] )
        actions
        |> Tuple.mapSecond List.reverse
