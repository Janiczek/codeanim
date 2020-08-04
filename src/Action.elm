module Action exposing
    ( Action
    , FadeOutOptions
    , RawAction(..)
    , TypeTextOptions
    , WaitOptions
    , bgColor
    , durationFrames
    , label
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

        FadeOutAndBlank _ ->
            E.rgb255 0xBB 0xCC 0xDD

        BlankText ->
            E.rgb255 0xB0 0xB0 0xBB

        SetText _ ->
            E.rgb255 0xBC 0xCD 0xC0


type RawAction
    = TypeText TypeTextOptions
    | Wait WaitOptions
    | FadeOutAndBlank FadeOutOptions
    | BlankText
    | SetText String


type alias Action =
    { raw : RawAction
    , startFrame : Int
    , endFrame : Int
    , index : Int
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

        FadeOutAndBlank r ->
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

        FadeOutAndBlank _ ->
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

        FadeOutAndBlank r ->
            "Fade out and blank text in " ++ Format.framesAsSeconds r.durationFrames

        BlankText ->
            "Blank text"

        SetText text ->
            "Set text: " ++ snippet text
