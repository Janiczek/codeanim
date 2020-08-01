module Todo exposing (actions)

import Action exposing (Action(..))


actions : List Action
actions =
    [ TypeText
        { text = "module Example exposing (example)\n\n\n"
        , duration = 1000
        , position = Nothing
        }
    , Wait { duration = 3000 }
    , TypeText
        { text = "import Minithesis\n"
        , duration = 700
        , position = Nothing
        }
    , Wait { duration = 300 }
    , TypeText
        { text = "import Minithesis.Generator as Gen\n\n\n"
        , duration = 700
        , position = Nothing
        }
    , Wait { duration = 1000 }
    , FadeOut { duration = 1000 }
    , Wait { duration = 500 }
    , TypeText
        { text =
            """example : Minithesis.TestResult
example =
    Minithesis.runWithSeed 0 test


"""
        , duration = 700
        , position = Nothing
        }
    ]
