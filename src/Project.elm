module Project exposing (Project, init, withActions)

import Action exposing (Action, RawAction)
import Element as E
import List.Extra


type alias Project =
    { actions : List Action
    , totalFrames : Int
    , endFrame : Int
    , codeBg : E.Color
    }


init :
    { actions : List RawAction
    , codeBg : E.Color
    }
    -> Project
init { actions, codeBg } =
    { codeBg = codeBg

    -- the rest will be set in `withActions`
    , actions = []
    , totalFrames = 0
    , endFrame = 0
    }
        |> withActions actions


withActions : List RawAction -> Project -> Project
withActions actions project =
    let
        ( totalFrames, processedActions ) =
            List.Extra.indexedFoldl
                (\index action ( accFrames, accActions ) ->
                    let
                        nextActionStartFrame =
                            accFrames + Action.durationFrames action
                    in
                    ( nextActionStartFrame
                    , { raw = action
                      , startFrame = accFrames
                      , endFrame = nextActionStartFrame - 1
                      , index = index
                      }
                        :: accActions
                    )
                )
                ( 0, [] )
                actions
                |> Tuple.mapSecond List.reverse
    in
    { project
        | actions = processedActions
        , totalFrames = totalFrames
        , endFrame = totalFrames - 1
    }
