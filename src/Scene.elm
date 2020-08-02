module Scene exposing
    ( Scene
    , compute
    )

import Action
    exposing
        ( Action
        , FadeOutOptions
        , RawAction(..)
        , TypeTextOptions
        )
import Config exposing (fps)
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


compute : Int -> Project -> Scene
compute currentFrame project =
    {- life's too short for optimizations, let's recompute every time -}
    let
        go : Int -> Action -> List Action -> Scene -> Scene
        go frame currentAction restOfActions accScene =
            if frame > currentFrame then
                accScene

            else if frame > currentAction.endFrame then
                case restOfActions of
                    [] ->
                        accScene

                    nextAction :: newRest ->
                        go
                            frame
                            nextAction
                            newRest
                            { accScene | opacity = 1 }

            else
                let
                    advancedScene : Scene
                    advancedScene =
                        case currentAction.raw of
                            TypeText r ->
                                goTypeText r frame currentAction accScene

                            Wait _ ->
                                accScene

                            FadeOutAndBlank r ->
                                goFadeOut r frame currentAction accScene

                            BlankText ->
                                { accScene | text = "" }

                            SetText text ->
                                { accScene | text = text }
                in
                go (frame + 1) currentAction restOfActions advancedScene

        goTypeText : TypeTextOptions -> Int -> Action -> Scene -> Scene
        goTypeText { text, durationFrames, position } frame currentAction scene =
            let
                previousLength : Int
                previousLength =
                    charsWritten
                        { stringLength = String.length text
                        , durationFrames = durationFrames
                        , currentFrame = frame - currentAction.startFrame - 1
                        }

                newLength : Int
                newLength =
                    charsWritten
                        { stringLength = String.length text
                        , durationFrames = durationFrames
                        , currentFrame = frame - currentAction.startFrame
                        }

                newText : String
                newText =
                    if previousLength /= newLength then
                        scene.text ++ String.slice previousLength newLength text

                    else
                        scene.text
            in
            { scene | text = newText }

        goFadeOut : FadeOutOptions -> Int -> Action -> Scene -> Scene
        goFadeOut { durationFrames } frame currentAction scene =
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
    in
    case project.actions of
        [] ->
            empty

        action :: rest ->
            go 0 action rest empty
