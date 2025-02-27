module Scene exposing
    ( Scene
    , advance
    , compute
    , toKey
    )

import Action
    exposing
        ( Action
        , FadeOutOptions
        , RawAction(..)
        , TypeTextOptions
        , TypeTextSpeedOptions
        )
import Config exposing (fps)
import Json.Encode
import List.Extra
import Project exposing (Project)
import Time


type alias Scene =
    { text : String
    , opacity : Float
    }


empty : Scene
empty =
    { text = ""
    , opacity = 1.0
    }


charsWritten :
    { stringLength : Int
    , durationFrames : Int
    , currentFrame : Int
    }
    -> Int
charsWritten { stringLength, durationFrames, currentFrame } =
    ceiling
        (toFloat stringLength
            * Time.percentage currentFrame durationFrames
        )


encode : Scene -> Json.Encode.Value
encode scene =
    [ ( "text", Json.Encode.string scene.text )
    , ( "opacity", Json.Encode.float scene.opacity )
    ]
        |> Json.Encode.object


toKey : Scene -> String
toKey scene =
    scene
        |> encode
        |> Json.Encode.encode 0


compute : Int -> Project -> Scene
compute currentFrame project =
    case project.actions of
        [] ->
            empty

        action :: rest ->
            computeFrom 0 currentFrame action rest empty


advance : Int -> Project -> Scene -> Scene
advance currentFrame project scene =
    case
        List.Extra.dropWhile
            (\{ endFrame } -> currentFrame > endFrame)
            project.actions
    of
        [] ->
            scene

        currentAction :: restOfActions ->
            computeFrom
                currentFrame
                project.endFrame
                currentAction
                restOfActions
                scene


computeFrom : Int -> Int -> Action -> List Action -> Scene -> Scene
computeFrom frame endFrame currentAction restOfActions accScene =
    if frame > endFrame then
        accScene

    else if frame > currentAction.endFrame then
        case restOfActions of
            [] ->
                accScene

            nextAction :: newRest ->
                computeFrom
                    frame
                    endFrame
                    nextAction
                    newRest
                    { accScene | opacity = 1 }

    else
        let
            advancedScene : Scene
            advancedScene =
                case currentAction.raw of
                    TypeText r ->
                        advanceTypeText r frame currentAction accScene

                    TypeTextSpeed r ->
                        advanceTypeTextSpeed r frame currentAction accScene

                    Wait _ ->
                        accScene

                    FadeOutAndBlank r ->
                        advanceFadeOut r frame currentAction accScene

                    BlankText ->
                        { accScene | text = "" }

                    SetText { text } ->
                        { accScene | text = text }
        in
        computeFrom (frame + 1) endFrame currentAction restOfActions advancedScene


advanceTypeText : TypeTextOptions -> Int -> Action -> Scene -> Scene
advanceTypeText { text, durationFrames, position } frame currentAction scene =
    let
        textLength : Int
        textLength =
            String.length text

        previousLength : Int
        previousLength =
            charsWritten
                { stringLength = textLength
                , durationFrames = durationFrames
                , currentFrame = frame - currentAction.startFrame - 1
                }

        newLength : Int
        newLength =
            if frame == currentAction.endFrame then
                textLength

            else
                charsWritten
                    { stringLength = textLength
                    , durationFrames = durationFrames
                    , currentFrame = frame - currentAction.startFrame
                    }

        newText : String
        newText =
            if previousLength /= newLength then
                case position of
                    Nothing ->
                        scene.text ++ String.slice previousLength newLength text

                    Just position_ ->
                        String.left (position_ + previousLength) scene.text
                            ++ String.slice previousLength newLength text
                            ++ String.dropLeft (position_ + previousLength) scene.text

            else
                scene.text
    in
    { scene | text = newText }


advanceTypeTextSpeed : TypeTextSpeedOptions -> Int -> Action -> Scene -> Scene
advanceTypeTextSpeed ({ text, charsPerSecond, position } as options) frame currentAction scene =
    let
        textLength : Int
        textLength =
            String.length text

        durationFrames : Int
        durationFrames =
            Action.durationFrames (TypeTextSpeed options)

        previousLength : Int
        previousLength =
            charsWritten
                { stringLength = textLength
                , durationFrames = durationFrames
                , currentFrame = frame - currentAction.startFrame - 1
                }

        newLength : Int
        newLength =
            if frame == currentAction.endFrame then
                textLength

            else
                charsWritten
                    { stringLength = textLength
                    , durationFrames = durationFrames
                    , currentFrame = frame - currentAction.startFrame
                    }

        newText : String
        newText =
            if previousLength /= newLength then
                case position of
                    Nothing ->
                        scene.text ++ String.slice previousLength newLength text

                    Just position_ ->
                        String.left (position_ + previousLength) scene.text
                            ++ String.slice previousLength newLength text
                            ++ String.dropLeft (position_ + previousLength) scene.text

            else
                scene.text
    in
    { scene | text = newText }


advanceFadeOut : FadeOutOptions -> Int -> Action -> Scene -> Scene
advanceFadeOut { durationFrames } frame currentAction scene =
    if frame == currentAction.endFrame then
        { scene
            | opacity = 0
            , text = ""
        }

    else
        { scene
            | opacity =
                1
                    - Time.percentage
                        (frame - currentAction.startFrame)
                        durationFrames
        }
