module Action exposing
    ( Action
    , FadeOutOptions
    , RawAction(..)
    , SetTextOptions
    , TypeTextOptions
    , TypeTextSpeedOptions
    , WaitOptions
    , bgColor
    , durationFrames
    , label
    , tooltip
    )

import Element as E
import Format
import Time


bgColor : RawAction -> E.Color
bgColor action =
    case action of
        TypeText _ ->
            E.rgb255 0xCC 0xDD 0xD0

        TypeTextSpeed _ ->
            E.rgb255 0xCC 0xDD 0xB0

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
    | TypeTextSpeed TypeTextSpeedOptions
    | Wait WaitOptions
    | FadeOutAndBlank FadeOutOptions
    | BlankText
    | SetText SetTextOptions


type alias Action =
    { raw : RawAction
    , startFrame : Int
    , endFrame : Int
    , index : Int
    }


type alias TypeTextOptions =
    { durationFrames : Int
    , position : Maybe Int -- Nothing = at the end
    , text : String
    }


type alias TypeTextSpeedOptions =
    { charsPerSecond : Int
    , position : Maybe Int -- Nothing = at the end
    , text : String
    }


type alias WaitOptions =
    { durationFrames : Int }


type alias FadeOutOptions =
    { durationFrames : Int }


type alias SetTextOptions =
    { text : String }


durationFrames : RawAction -> Int
durationFrames action =
    case action of
        TypeText r ->
            r.durationFrames

        TypeTextSpeed r ->
            let
                ms =
                    String.length r.text * 1000 // r.charsPerSecond
            in
            Time.ms ms

        Wait r ->
            r.durationFrames

        FadeOutAndBlank r ->
            r.durationFrames

        BlankText ->
            {- TODO make this ms-based so that it takes the same amount of time
               with different FPS settings
            -}
            Time.ms 500

        SetText _ ->
            {- TODO make this ms-based so that it takes the same amount of time
               with different FPS settings
            -}
            Time.ms 500


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

        TypeTextSpeed _ ->
            "Type (speed)"

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
            "Type: " ++ snippet text

        TypeTextSpeed { text } ->
            "Type (speed): " ++ snippet text

        Wait r ->
            "Wait " ++ Format.framesAsSeconds r.durationFrames

        FadeOutAndBlank r ->
            "Fade out and blank text in " ++ Format.framesAsSeconds r.durationFrames

        BlankText ->
            "Blank text"

        SetText { text } ->
            "Set text: " ++ snippet text
