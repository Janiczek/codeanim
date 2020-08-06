module Project exposing (Project, empty, withActions)

import Action exposing (Action, RawAction)
import Element as E
import List.Extra


type alias Project =
    { actions : List Action
    , totalFrames : Int
    , endFrame : Int
    }


empty : Project
empty =
    { actions = []
    , totalFrames = 0
    , endFrame = -1
    }


withActions : List RawAction -> Project
withActions actions =
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
    { actions = processedActions
    , totalFrames = totalFrames
    , endFrame = totalFrames - 1
    }
