module Todo exposing (project)

import Action exposing (RawAction(..))
import Element as E
import Project exposing (Project)
import Time


project : Project
project =
    Project.init
        { actions = actions
        , codeBg = E.rgb255 0x33 0x33 0x33
        , codeColor = E.rgb255 0xF0 0xF0 0xF0
        }


actions : List RawAction
actions =
    [ TypeText
        { text = "module Example exposing (example)\n\n\n"
        , durationFrames = ceiling <| Time.msToFrame 1000
        , position = Nothing
        }
    , Wait { durationFrames = ceiling <| Time.msToFrame 3000 }
    , TypeText
        { text = "import Minithesis\n"
        , durationFrames = ceiling <| Time.msToFrame 700
        , position = Nothing
        }
    , Wait { durationFrames = ceiling <| Time.msToFrame 300 }
    , TypeText
        { text = "import Minithesis.Generator as Gen\n\n\n"
        , durationFrames = ceiling <| Time.msToFrame 700
        , position = Nothing
        }
    , Wait { durationFrames = ceiling <| Time.msToFrame 1000 }
    , FadeOut { durationFrames = ceiling <| Time.msToFrame 1000 }
    , Wait { durationFrames = ceiling <| Time.msToFrame 500 }
    , TypeText
        { text =
            """example : Minithesis.TestResult
example =
    Minithesis.runWithSeed 0 test


"""
        , durationFrames = ceiling <| Time.msToFrame 700
        , position = Nothing
        }
    ]
