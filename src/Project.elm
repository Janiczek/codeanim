module Project exposing (Project, init)

import Action exposing (Action, RawAction)
import Element as E


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
    let
        ( totalFrames, processedActions ) =
            Action.process actions
    in
    { actions = processedActions
    , totalFrames = totalFrames
    , endFrame = totalFrames - 1
    , codeBg = codeBg
    }
