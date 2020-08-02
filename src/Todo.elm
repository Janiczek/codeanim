module Todo exposing (project)

import Action exposing (RawAction(..))
import Element as E
import Project exposing (Project)
import Time


project : Project
project =
    Project.init
        { actions = actions
        , codeBg = E.rgb255 0x21 0x27 0x33
        }


ms : Int -> Int
ms ms_ =
    ceiling <| Time.msToFrame (toFloat ms_)


actions : List RawAction
actions =
    [ Wait
        { durationFrames = ms 500 }
    , TypeText
        { text = "module Example exposing (example)\n\n\n"
        , durationFrames = ms 2000
        , position = Nothing
        }
    , Wait
        { durationFrames = ms 2000 }
    , TypeText
        { text = "import Minithesis\n"
        , durationFrames = ms 700
        , position = Nothing
        }
    , Wait
        { durationFrames = ms 300 }
    , TypeText
        { text = "import Minithesis.Generator as Gen\n\n\n"
        , durationFrames = ms 700
        , position = Nothing
        }
    , Wait
        { durationFrames = ms 500 }

    --, FadeOut
    --{ durationFrames = ms 1000 }
    --, Wait
    --{ durationFrames = ms 1000 }
    , TypeText
        { text =
            """example : Minithesis.TestResult
example =
    Minithesis.runWithSeed 0 test


"""
        , durationFrames = ms 700
        , position = Nothing
        }
    ]
