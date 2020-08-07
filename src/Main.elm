port module Main exposing (main)

import Action
    exposing
        ( Action
        , FadeOutOptions
        , RawAction(..)
        , SetTextOptions
        , TypeTextOptions
        , WaitOptions
        )
import Browser
import Browser.Events
import Config exposing (fps, fpsF)
import DnDList
import Element as E exposing (Element)
import Element.Background as EBg
import Element.Border as EBo
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import FeatherIcons as FI exposing (Icon)
import Format
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Wheel
import Json.Decode exposing (Decoder)
import Json.Encode
import List.Extra
import List.Zipper as Zipper exposing (Zipper)
import Markdown
import Parser exposing ((|.), (|=), Parser)
import Process
import Project exposing (Project)
import Scene exposing (Scene)
import Svg
import Svg.Attributes
import Task
import Time
import Zoom


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port goFullscreen : () -> Cmd msg


port exitFullscreen : () -> Cmd msg


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


type alias Model =
    { zoom : Int

    -- TODO , currentScene : Scene -> use `Scene.advance` later when possible
    , currentFrame : Int
    , project : Project
    , lastParseUnsuccessful : Bool
    , state : State
    , leftoverDelta : Float
    , hoveringAtFrame : Maybe Int
    , dnd : DnDList.Model
    , userText : String
    , timelineScrollX : Int
    }


type State
    = Paused
    | Playing
    | StartingFullscreen
    | PlayingFullscreen
    | EndingFullscreen
    | Rendering


type Msg
    = ZoomIn
    | ZoomOut
    | ResetZoom
    | JumpForward Int
    | JumpBackward Int
    | JumpToPrevious
    | JumpToNext
    | JumpToStart
    | JumpToEnd
    | Play
    | Pause
    | Tick Float
    | HoverAtPx Int
    | HoverOff
    | JumpToFrameAtPx Int
    | GoFullscreenAndPlay
    | StartRendering
    | AddAction RawAction
    | RemoveActionAtIndex Int
    | NoOp
    | DnDMsg DnDList.Msg
    | Load String
    | SetScrollX Int


toString_ : Project -> String
toString_ project =
    project.actions
        |> List.map .raw
        |> toString


toString : List RawAction -> String
toString actions =
    (actions
        |> List.map actionToString
        |> String.join "\n"
    )
        ++ "\n##### END"


actionToString : RawAction -> String
actionToString action =
    case action of
        TypeText r ->
            [ "##### TypeText "
                ++ String.fromInt (Time.frameToMs_ r.durationFrames)
                ++ "ms"
                ++ (case r.position of
                        Nothing ->
                            ""

                        Just position ->
                            " at " ++ String.fromInt position
                   )
            , r.text
            ]
                |> String.join "\n"

        Wait r ->
            "##### Wait "
                ++ String.fromInt (Time.frameToMs_ r.durationFrames)
                ++ "ms"

        FadeOutAndBlank r ->
            "##### FadeOut "
                ++ String.fromInt (Time.frameToMs_ r.durationFrames)
                ++ "ms"

        BlankText ->
            "##### Blank"

        SetText r ->
            [ "##### SetText"
            , r.text
            ]
                |> String.join "\n"


parse : String -> Maybe (List RawAction)
parse string =
    Parser.run parser string
        |> Result.toMaybe


parser : Parser (List RawAction)
parser =
    Parser.succeed identity
        |= Parser.sequence
            { start = ""
            , separator = "\n"
            , end = "##### END"
            , spaces = Parser.succeed ()
            , item = actionParser
            , trailing = Parser.Optional
            }
        |. Parser.end


actionParser : Parser RawAction
actionParser =
    Parser.oneOf
        [ typeTextParser
        , waitParser
        , fadeOutParser
        , blankParser
        , setTextParser
        ]


{-|

    ##### TypeText 300ms
    blabla

    def

-}
typeTextParser : Parser RawAction
typeTextParser =
    Parser.succeed TypeTextOptions
        |. Parser.token "##### TypeText "
        |= Parser.map Time.ms Parser.int
        |. Parser.token "ms"
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.token " at "
                |= Parser.int
            , Parser.succeed Nothing
            ]
        |. Parser.token "\n"
        |= Parser.getChompedString (Parser.chompUntil "\n#####")
        |> Parser.map TypeText


{-|

    ##### Wait 300 ms

-}
waitParser : Parser RawAction
waitParser =
    Parser.succeed WaitOptions
        |. Parser.token "##### Wait "
        |= Parser.map Time.ms Parser.int
        |. Parser.token "ms"
        |> Parser.map Wait


{-|

    ##### FadeOut 300 ms

-}
fadeOutParser : Parser RawAction
fadeOutParser =
    Parser.succeed FadeOutOptions
        |. Parser.token "##### FadeOut "
        |= Parser.map Time.ms Parser.int
        |. Parser.token "ms"
        |> Parser.map FadeOutAndBlank


{-|

    ##### Blank

-}
blankParser : Parser RawAction
blankParser =
    Parser.succeed BlankText
        |. Parser.token "##### Blank"


{-|

    ##### SetText
    blabla

    def

-}
setTextParser : Parser RawAction
setTextParser =
    Parser.succeed SetTextOptions
        |. Parser.token "##### SetText\n"
        |= Parser.getChompedString (Parser.chompUntil "\n#####")
        |> Parser.map SetText


init : () -> ( Model, Cmd Msg )
init () =
    ( { zoom = 0
      , currentFrame = 0
      , project = Project.empty
      , lastParseUnsuccessful = False
      , state = Paused
      , hoveringAtFrame = Nothing
      , leftoverDelta = 0
      , dnd = dndSystem.model
      , userText = ""
      , timelineScrollX = 0
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    E.layout
        [ EBg.color (E.rgb255 0x70 0x70 0x70)
        , E.width E.fill
        , E.height E.fill
        ]
        (case model.state of
            Paused ->
                viewEdit model

            Playing ->
                viewEdit model

            StartingFullscreen ->
                viewFullscreen model

            PlayingFullscreen ->
                viewFullscreen model

            EndingFullscreen ->
                viewFullscreen model

            Rendering ->
                viewFullscreen model
        )


viewFullscreen : Model -> Element Msg
viewFullscreen model =
    E.el
        [ E.htmlAttribute (Html.Attributes.id "fullscreen-scene")
        , E.width E.fill
        , E.height E.fill
        ]
        (viewSceneForFrame model model.currentFrame)


viewEdit : Model -> Element Msg
viewEdit model =
    E.column
        [ E.width E.fill
        , E.height E.fill
        , E.clip
        ]
        [ E.row
            [ E.width E.fill
            , E.height E.fill
            ]
            [ viewPreview model
            , viewTextRepresentation model
            ]
        , viewTimeline model
        ]


viewTextRepresentation :
    { a
        | project : Project
        , lastParseUnsuccessful : Bool
        , userText : String
    }
    -> Element Msg
viewTextRepresentation { project, lastParseUnsuccessful, userText } =
    E.html <|
        Html.textarea
            [ Html.Attributes.style "width" "400px"
            , Html.Attributes.style "font-family" "\"Iosevka Janiczek Web\", monospace"
            , Html.Attributes.style "font-size" "14px"
            , Html.Attributes.style "height" "calc(100% - 20px)"
            , Html.Attributes.style "margin" "10px"
            , Html.Attributes.style "resize" "none"
            , Html.Attributes.style "background-color"
                (if lastParseUnsuccessful then
                    "#FF9988"

                 else
                    "white"
                )
            , Html.Events.onInput Load
            , Html.Attributes.value userText
            ]
            []


viewPreview :
    { a
        | currentFrame : Int
        , project : Project
        , hoveringAtFrame : Maybe Int
        , dnd : DnDList.Model
    }
    -> Element Msg
viewPreview ({ project, currentFrame, hoveringAtFrame, dnd } as model) =
    let
        frame : Int
        frame =
            if dndSystem.info dnd == Nothing then
                hoveringAtFrame
                    |> Maybe.map (min project.totalFrames)
                    |> Maybe.withDefault currentFrame

            else
                currentFrame
    in
    E.el
        [ E.width E.fill
        , E.height E.fill
        , E.centerX
        , E.centerY
        ]
        (E.el
            [ E.width E.fill
            , E.height E.fill
            , E.htmlAttribute (Html.Attributes.style "max-width" "min(1200px, 80vw)")
            , E.centerX
            , E.centerY
            , E.padding 20
            ]
            (E.el
                [ EBg.color (E.rgb255 0xE0 0xE0 0xE0)
                , E.centerY
                , E.htmlAttribute (Html.Attributes.style "position" "relative")
                , E.htmlAttribute (Html.Attributes.style "width" "100%")
                , E.htmlAttribute (Html.Attributes.style "height" "0")
                , E.htmlAttribute (Html.Attributes.style "padding-top" "56.25%")
                , EBo.shadow
                    { offset = ( 1, 1 )
                    , size = 0
                    , blur = 5
                    , color = E.rgba255 0x10 0x00 0x00 0.75
                    }
                ]
                (viewSceneForFrame model frame)
            )
        )


viewSceneForFrame :
    { a | project : Project }
    -> Int
    -> Element Msg
viewSceneForFrame ({ project } as model) frame =
    let
        scene : Scene
        scene =
            Scene.compute frame project
    in
    viewScene model scene


viewScene : { a | project : Project } -> Scene -> Element Msg
viewScene { project } scene =
    E.el
        [ EBg.color (E.rgb255 0x21 0x27 0x33)
        , E.htmlAttribute (Html.Attributes.style "position" "absolute")
        , E.htmlAttribute (Html.Attributes.style "width" "100%")
        , E.htmlAttribute (Html.Attributes.style "height" "100%")
        , E.htmlAttribute (Html.Attributes.style "top" "0")
        , E.htmlAttribute (Html.Attributes.style "left" "0")
        ]
        (E.el
            [ E.alpha scene.opacity
            , E.width E.fill
            , E.height E.fill
            ]
            (E.html <|
                Svg.svg
                    [ Svg.Attributes.width "100%"
                    , Svg.Attributes.viewBox "0 0 1920 1080"
                    ]
                    [ Svg.foreignObject
                        [ Svg.Attributes.width "100%"
                        , Svg.Attributes.height "100%"

                        -- holds 10 lines of text
                        , Svg.Attributes.x "130"
                        , Svg.Attributes.y "80"
                        ]
                        [ Html.node "x-highlight"
                            [ Html.Attributes.attribute "data-code" scene.text
                            , Html.Attributes.style "font-size" "60px"
                            ]
                            []
                        ]
                    ]
            )
        )


viewTimeline :
    { a
        | currentFrame : Int
        , zoom : Int
        , project : Project
        , state : State
        , hoveringAtFrame : Maybe Int
        , dnd : DnDList.Model
        , timelineScrollX : Int
    }
    -> Element Msg
viewTimeline ({ currentFrame, zoom, project, state, hoveringAtFrame, dnd } as model) =
    E.column
        [ E.width E.fill
        , EBg.color (E.rgb255 0x50 0x50 0x50)
        , EBo.widthEach
            { top = 1
            , bottom = 0
            , left = 0
            , right = 0
            }
        , EBo.color (E.rgb255 0x33 0x33 0x33)
        , E.clip
        , E.htmlAttribute <|
            Html.Events.Extra.Wheel.onWheel
                (\event ->
                    if event.deltaY > 0 then
                        ZoomOut

                    else
                        ZoomIn
                )
        , E.htmlAttribute (Html.Attributes.style "user-select" "none")
        ]
        [ E.column
            [ if dndSystem.info dnd == Nothing then
                E.inFront <|
                    case hoveringAtFrame of
                        Just frame ->
                            viewFrameMarker
                                model
                                frame
                                (E.rgb255 0xFF 0xFF 0xFF)

                        Nothing ->
                            E.none

              else
                emptyAttr
            , E.inFront <|
                viewFrameMarker
                    model
                    currentFrame
                    (E.rgb255 0xFF 0x00 0x00)
            , E.width E.fill
            , E.htmlAttribute (Html.Attributes.style "overflow-x" "scroll")
            , E.htmlAttribute (Html.Events.on "scroll" (onScroll SetScrollX))
            , E.htmlAttribute (Html.Events.on "mousemove" currentPxDecoder)
                |> E.mapAttribute HoverAtPx
            , E.htmlAttribute (Html.Events.on "click" currentPxDecoder)
                |> E.mapAttribute JumpToFrameAtPx
            , E.htmlAttribute (Html.Events.onMouseOut HoverOff)
            ]
            [ viewSecondsRuler model
            , viewActions model
            ]
        , viewTimelineControls model
        , viewAddButtons
        ]


viewSecondsRuler :
    { a
        | zoom : Int
        , project : Project
    }
    -> Element Msg
viewSecondsRuler ({ zoom, project } as model) =
    E.el
        ([ E.height (E.px 20)
         , EF.size 14
         , E.width E.fill
         , EBo.widthEach
            { bottom = 1
            , top = 0
            , left = 0
            , right = 0
            }
         , EBo.color (E.rgb255 0x33 0x33 0x33)
         ]
            ++ List.map
                (viewRulerMarker model >> E.inFront)
                (List.range 0 (project.totalFrames // fps + 1))
        )
        E.none


viewRulerMarker :
    { a | zoom : Int }
    -> Int
    -> Element Msg
viewRulerMarker { zoom } i =
    let
        frame =
            i * fps
    in
    E.el
        [ E.height (E.px 20)
        , EF.color (E.rgb255 0x90 0x90 0x90)
        , E.paddingEach
            { left = 5
            , top = 0
            , right = 0
            , bottom = 0
            }
        , EBo.widthEach
            { left = 1
            , top = 0
            , right = 0
            , bottom = 0
            }
        , E.moveRight
            (toFloat
                (Zoom.frameToPx
                    { frame = frame
                    , zoom = zoom
                    }
                )
            )
        ]
        (E.el
            [ E.centerY ]
            (E.text <| String.fromInt i ++ "s")
        )


viewActions :
    { a
        | project : Project
        , zoom : Int
        , hoveringAtFrame : Maybe Int
        , dnd : DnDList.Model
    }
    -> Element Msg
viewActions ({ project, dnd } as model) =
    E.row
        [ E.width E.fill
        , E.height (E.px 70)
        , E.inFront (viewActionGhost model)
        ]
        (List.indexedMap (viewAction model) project.actions)


viewActionGhost :
    { a
        | zoom : Int
        , project : Project
        , dnd : DnDList.Model
        , hoveringAtFrame : Maybe Int
    }
    -> Element Msg
viewActionGhost ({ project, dnd } as model) =
    dndSystem.info dnd
        |> Maybe.andThen (\{ dragIndex } -> List.Extra.getAt dragIndex project.actions)
        |> Maybe.map
            (\action ->
                viewActionWith
                    (List.map E.htmlAttribute (dndSystem.ghostStyles dnd))
                    Nothing
                    model
                    action
            )
        |> Maybe.withDefault E.none


viewFrameMarker :
    { a
        | zoom : Int
        , timelineScrollX : Int
    }
    -> Int
    -> E.Color
    -> Element Msg
viewFrameMarker { zoom, timelineScrollX } frame color =
    E.el
        [ E.moveRight
            (toFloat
                (Zoom.frameToPx
                    { frame = frame
                    , zoom = zoom
                    }
                )
            )
        , E.htmlAttribute (Html.Attributes.style "pointer-events" "none")
        , E.width (E.px 1)
        , E.height E.fill
        , EBg.color color
        ]
        E.none


viewTimelineControls :
    { a
        | currentFrame : Int
        , state : State
        , project : Project
    }
    -> Element Msg
viewTimelineControls { currentFrame, state, project } =
    E.row
        [ E.spaceEvenly
        , E.width E.fill
        , E.height E.fill
        , EBo.widthEach
            { top = 1
            , bottom = 0
            , left = 0
            , right = 0
            }
        , EBo.color (E.rgb255 0x33 0x33 0x33)
        ]
        [ E.el []
            (E.row [ E.spacing 2 ]
                [ viewButton JumpToStart FI.cornerLeftDown "Jump to start"
                , viewButton JumpToPrevious FI.skipBack "Jump to previous action"
                , viewButton (JumpBackward 10) FI.chevronsLeft "Jump backward (10f)"
                , viewButton (JumpBackward 1) FI.chevronLeft "Step backward"
                , case state of
                    Paused ->
                        viewButton Play FI.play "Play"

                    Playing ->
                        viewButton Pause FI.pause "Pause"

                    StartingFullscreen ->
                        E.none

                    PlayingFullscreen ->
                        E.none

                    EndingFullscreen ->
                        E.none

                    Rendering ->
                        E.none
                , viewButton GoFullscreenAndPlay FI.playCircle "Play from start in fullscreen"
                , viewButton (JumpForward 1) FI.chevronRight "Step forward"
                , viewButton (JumpForward 10) FI.chevronsRight "Jump forward (10f)"
                , viewButton JumpToNext FI.skipForward "Jump to next action"
                , viewButton JumpToEnd FI.cornerRightDown "Jump to end"
                ]
            )
        , E.el
            [ E.centerY
            , EF.color (E.rgb255 0xBB 0xBB 0xBB)
            ]
            (E.text <|
                "Frame: "
                    ++ String.fromInt currentFrame
                    ++ " ("
                    ++ Format.framesAsSeconds currentFrame
                    ++ ")"
            )
        , E.el []
            (E.row [ E.spacing 2 ]
                [ viewButton ZoomOut FI.zoomOut "Zoom out"
                , viewButton ResetZoom FI.monitor "Reset zoom"
                , viewButton ZoomIn FI.zoomIn "Zoom in"
                ]
            )
        ]


viewAddButtons : Element Msg
viewAddButtons =
    E.row
        [ E.spaceEvenly
        , E.width E.fill
        , E.height E.fill
        , EBo.widthEach
            { top = 1
            , bottom = 0
            , left = 0
            , right = 0
            }
        , EBo.color (E.rgb255 0x33 0x33 0x33)
        ]
        [ {- E.el []
             (
          -}
          E.row [ E.spacing 2 ]
            [ viewButton
                (AddAction
                    (TypeText
                        { text = "Text"
                        , durationFrames = Time.ms 2000
                        , position = Nothing
                        }
                    )
                )
                FI.type_
                "Add 'Type' action"
            , viewButton
                (AddAction
                    (Wait
                        { durationFrames = Time.ms 2000 }
                    )
                )
                FI.watch
                "Add 'Wait' action"
            , viewButton
                (AddAction
                    (FadeOutAndBlank
                        { durationFrames = Time.ms 500 }
                    )
                )
                FI.loader
                -- FI.minimize
                "Add 'Fade out' action"
            , viewButton
                (AddAction BlankText)
                FI.square
                "Add 'Blank' action"
            , viewButton
                (AddAction (SetText { text = "Text" }))
                FI.alignLeft
                "Add 'Set Text' action"
            ]

        -- )
        ]


viewButton : Msg -> Icon -> String -> Element Msg
viewButton msg icon tooltip =
    EI.button
        [ E.padding 5
        , EBg.color (E.rgb255 0xDD 0xDD 0xCC)
        , E.width (E.px 30)
        , EF.center
        , E.htmlAttribute (Html.Attributes.title tooltip)
        ]
        { onPress = Just msg
        , label = E.html (FI.toHtml [] icon)
        }


viewAction :
    { a
        | zoom : Int
        , hoveringAtFrame : Maybe Int
        , project : Project
        , dnd : DnDList.Model
    }
    -> Int
    -> Action
    -> Element Msg
viewAction model index action =
    viewActionWith [] (Just index) model action


viewActionWith :
    List (E.Attribute Msg)
    -> Maybe Int
    ->
        { a
            | zoom : Int
            , dnd : DnDList.Model
            , project : Project
            , hoveringAtFrame : Maybe Int
        }
    -> Action
    -> Element Msg
viewActionWith attrs maybeIndex { hoveringAtFrame, project, zoom, dnd } action =
    let
        color =
            Action.bgColor action.raw

        hoveringThisAction : Bool
        hoveringThisAction =
            hoveringAtFrame
                |> Maybe.andThen (actionIndexForFrame project)
                |> Maybe.map (\i -> Just i == maybeIndex)
                |> Maybe.withDefault False

        id =
            case maybeIndex of
                Nothing ->
                    "ghost-action"

                Just index ->
                    "action-" ++ String.fromInt index

        ( dndEvents, opacity ) =
            case maybeIndex of
                Nothing ->
                    -- this is the ghost element
                    ( [], 0.75 )

                Just index ->
                    case dndSystem.info dnd of
                        Just { dragIndex } ->
                            if dragIndex /= index then
                                -- dragging over some other element
                                ( dndSystem.dropEvents index id, 1 )

                            else
                                -- dragging over this element
                                ( [], 0.5 )

                        Nothing ->
                            -- not dragging at all
                            ( dndSystem.dragEvents index id, 1 )
    in
    E.el
        (EBg.color color
            :: E.height (E.px 70)
            :: E.width
                (E.px
                    (Zoom.frameToPx
                        { frame = Action.durationFrames action.raw
                        , zoom = zoom
                        }
                    )
                )
            :: E.inFront
                (case maybeIndex of
                    Nothing ->
                        E.none

                    Just index ->
                        if hoveringThisAction then
                            EI.button
                                [ E.padding 2
                                , EF.color (E.rgb255 0xFF 0xFF 0xFF)
                                , EF.center
                                , E.alignRight
                                ]
                                { onPress = Just (RemoveActionAtIndex index)
                                , label = E.html (FI.toHtml [] FI.x)
                                }

                        else
                            E.none
                )
            :: E.clip
            :: E.alpha opacity
            :: EBo.width 2
            :: EBo.color (darken 0.1 color)
            :: EBo.rounded 4
            :: EF.color (darken 0.3 color)
            :: EF.size 14
            :: E.htmlAttribute (Html.Attributes.id id)
            :: E.htmlAttribute (Html.Attributes.title (Action.tooltip action.raw))
            :: List.map E.htmlAttribute dndEvents
            ++ attrs
        )
        (viewActionText action.raw)


currentPxDecoder : Decoder Int
currentPxDecoder =
    Json.Decode.field "clientX" Json.Decode.int


darken : Float -> E.Color -> E.Color
darken amount color =
    let
        { red, blue, green, alpha } =
            E.toRgb color
    in
    E.fromRgb
        { red = red - amount
        , green = green - amount
        , blue = blue - amount
        , alpha = alpha
        }


viewActionText : RawAction -> Element Msg
viewActionText action =
    E.el
        [ E.clip
        , E.width E.fill
        , E.height E.fill
        , E.htmlAttribute (Html.Attributes.style "user-select" "none")
        ]
        (E.el
            [ E.paddingEach
                { left = 5
                , top = 5
                , bottom = 0
                , right = 0
                }
            ]
            (E.text (Action.label action))
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ZoomIn ->
            ( { model | zoom = model.zoom + 1 }
            , Cmd.none
            )

        ZoomOut ->
            ( { model | zoom = model.zoom - 1 }
            , Cmd.none
            )

        ResetZoom ->
            ( { model | zoom = 0 }
            , Cmd.none
            )

        JumpForward n ->
            let
                newFrame =
                    model.currentFrame + n

                newState =
                    case model.state of
                        Paused ->
                            model.state

                        Playing ->
                            model.state

                        StartingFullscreen ->
                            model.state

                        PlayingFullscreen ->
                            model.state

                        EndingFullscreen ->
                            model.state

                        Rendering ->
                            if newFrame > model.project.endFrame then
                                Paused

                            else
                                model.state
            in
            ( { model
                | currentFrame = newFrame
                , state = newState
              }
            , Cmd.none
            )

        JumpBackward n ->
            ( { model | currentFrame = model.currentFrame - n }
            , Cmd.none
            )

        JumpToNext ->
            ( { model
                | currentFrame =
                    nextActionFrame
                        model.currentFrame
                        model.project
              }
            , Cmd.none
            )

        JumpToPrevious ->
            ( { model
                | currentFrame =
                    previousActionFrame
                        model.currentFrame
                        model.project
              }
            , Cmd.none
            )

        JumpToStart ->
            ( { model | currentFrame = 0 }
            , Cmd.none
            )

        JumpToEnd ->
            ( { model | currentFrame = model.project.totalFrames }
            , Cmd.none
            )

        Play ->
            ( { model
                | state =
                    case model.state of
                        Paused ->
                            Playing

                        Playing ->
                            Playing

                        StartingFullscreen ->
                            PlayingFullscreen

                        PlayingFullscreen ->
                            PlayingFullscreen

                        EndingFullscreen ->
                            Playing

                        Rendering ->
                            PlayingFullscreen
              }
            , Cmd.none
            )

        Pause ->
            ( { model | state = Paused }
            , case model.state of
                Playing ->
                    Cmd.none

                Paused ->
                    Cmd.none

                StartingFullscreen ->
                    exitFullscreen ()

                PlayingFullscreen ->
                    exitFullscreen ()

                EndingFullscreen ->
                    exitFullscreen ()

                Rendering ->
                    Cmd.none
            )

        Tick delta ->
            let
                play () =
                    let
                        delta_ =
                            delta + model.leftoverDelta

                        advancedFrames =
                            floor <| Time.msToFrame delta_

                        advancedCurrentFrame =
                            model.currentFrame + advancedFrames

                        hasEnded =
                            advancedCurrentFrame >= model.project.totalFrames

                        newLeftoverDelta =
                            if hasEnded then
                                0

                            else
                                delta_ - Time.frameToMs advancedFrames

                        newCurrentFrame =
                            if hasEnded then
                                model.project.totalFrames

                            else
                                advancedCurrentFrame

                        ( newState, maybeExitFullscreenCmd ) =
                            if hasEnded then
                                case model.state of
                                    Playing ->
                                        ( Paused, Cmd.none )

                                    PlayingFullscreen ->
                                        ( EndingFullscreen
                                        , Process.sleep 2000
                                            |> Task.andThen (\() -> Task.succeed Pause)
                                            |> Task.perform identity
                                        )

                                    Paused ->
                                        ( Paused, Cmd.none )

                                    StartingFullscreen ->
                                        ( Paused, Cmd.none )

                                    EndingFullscreen ->
                                        ( Paused, Cmd.none )

                                    Rendering ->
                                        ( Paused, Cmd.none )

                            else
                                ( model.state
                                , Cmd.none
                                )
                    in
                    ( { model
                        | leftoverDelta = newLeftoverDelta
                        , currentFrame = newCurrentFrame
                        , state = newState
                      }
                    , maybeExitFullscreenCmd
                    )
            in
            case model.state of
                Playing ->
                    play ()

                Paused ->
                    ( model, Cmd.none )

                StartingFullscreen ->
                    ( model, Cmd.none )

                PlayingFullscreen ->
                    play ()

                EndingFullscreen ->
                    ( model, Cmd.none )

                Rendering ->
                    ( model, Cmd.none )

        HoverAtPx px ->
            let
                frame =
                    Zoom.pxToFrame
                        { px = px + model.timelineScrollX
                        , zoom = model.zoom
                        }
            in
            ( { model | hoveringAtFrame = Just frame }
            , Cmd.none
            )

        HoverOff ->
            ( { model | hoveringAtFrame = Nothing }
            , Cmd.none
            )

        JumpToFrameAtPx px ->
            let
                frame =
                    Zoom.pxToFrame
                        { px = px + model.timelineScrollX
                        , zoom = model.zoom
                        }
            in
            ( { model | currentFrame = frame }
            , Cmd.none
            )

        GoFullscreenAndPlay ->
            ( { model
                | currentFrame = 0
                , state = StartingFullscreen
              }
            , Cmd.batch
                [ goFullscreen ()
                , Process.sleep 2000
                    |> Task.andThen (\() -> Task.succeed Play)
                    |> Task.perform identity
                ]
            )

        StartRendering ->
            ( { model
                | currentFrame = 0
                , state = Rendering
              }
            , Cmd.none
            )

        AddAction rawAction ->
            let
                rawActions =
                    model.project.actions
                        |> List.map .raw

                newProject =
                    Project.withActions (rawActions ++ [ rawAction ])

                projectString =
                    toString_ newProject
            in
            ( { model
                | project = newProject
                , lastParseUnsuccessful = False
                , userText = projectString
              }
            , save projectString
            )

        RemoveActionAtIndex index ->
            let
                rawActions =
                    model.project.actions
                        |> List.map .raw

                newRawActions =
                    List.take index rawActions
                        ++ List.drop (index + 1) rawActions

                newProject =
                    Project.withActions newRawActions

                projectString =
                    toString_ newProject
            in
            ( { model
                | project = newProject
                , lastParseUnsuccessful = False
                , userText = projectString
              }
            , save projectString
            )

        NoOp ->
            ( model, Cmd.none )

        DnDMsg subMsg ->
            let
                ( newDnd, actionsAfterDnd ) =
                    dndSystem.update subMsg model.dnd model.project.actions

                newProject =
                    Project.withActions
                        (List.map .raw actionsAfterDnd)

                projectString =
                    toString_ newProject
            in
            ( { model
                | dnd = newDnd
                , project = newProject
                , userText = projectString
              }
            , Cmd.batch
                [ dndSystem.commands newDnd
                , save projectString
                ]
            )

        Load string ->
            parse string
                |> Maybe.map
                    (\rawActions ->
                        ( { model
                            | lastParseUnsuccessful = False
                            , project = Project.withActions rawActions
                            , userText = string
                          }
                        , save string
                        )
                    )
                |> Maybe.withDefault
                    ( { model
                        | lastParseUnsuccessful = True
                        , userText = string
                      }
                    , Cmd.none
                    )

        SetScrollX x ->
            ( { model | timelineScrollX = x }
            , Cmd.none
            )


previousActionFrame : Int -> Project -> Int
previousActionFrame currentFrame { actions } =
    let
        go : Int -> List Action -> Int
        go accFrame actions_ =
            case actions_ of
                [] ->
                    accFrame

                action :: rest ->
                    let
                        durationFrames =
                            Action.durationFrames action.raw

                        newAccFrame =
                            accFrame + durationFrames
                    in
                    if newAccFrame >= currentFrame then
                        accFrame

                    else
                        go newAccFrame rest
    in
    go 0 actions


nextActionFrame : Int -> Project -> Int
nextActionFrame currentFrame { actions } =
    let
        go : Int -> List Action -> Int
        go accFrame actions_ =
            case actions_ of
                [] ->
                    accFrame

                action :: rest ->
                    let
                        durationFrames =
                            Action.durationFrames action.raw

                        newAccFrame =
                            accFrame + durationFrames
                    in
                    if newAccFrame > currentFrame then
                        newAccFrame

                    else
                        go newAccFrame rest
    in
    go 0 actions


actionIndexForFrame : Project -> Int -> Maybe Int
actionIndexForFrame project frame =
    List.Extra.find
        (\action -> action.endFrame >= frame)
        project.actions
        |> Maybe.map .index


onKeyDown : String -> Msg -> Decoder Msg
onKeyDown key msg =
    Json.Decode.field "code" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                if string == key then
                    Json.Decode.succeed msg

                else
                    Json.Decode.fail "don't care"
            )


onScroll : (Int -> Msg) -> Decoder Msg
onScroll toMsg =
    Json.Decode.at [ "target", "scrollLeft" ] Json.Decode.int
        |> Json.Decode.map toMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        listenForTick () =
            Browser.Events.onAnimationFrameDelta Tick

        listenForKey key msg =
            Browser.Events.onKeyDown (onKeyDown key msg)
    in
    case model.state of
        Paused ->
            Sub.batch
                [ listenForKey "F1" Play
                , listenForKey "F2" StartRendering
                , dndSystem.subscriptions model.dnd
                , load Load
                ]

        Playing ->
            Sub.batch
                [ listenForKey "F1" Pause
                , listenForKey "F2" StartRendering
                , dndSystem.subscriptions model.dnd
                , listenForTick ()
                ]

        StartingFullscreen ->
            listenForKey "F1" Pause

        PlayingFullscreen ->
            Sub.batch
                [ listenForTick ()
                , listenForKey "F1" Pause
                ]

        EndingFullscreen ->
            listenForKey "F1" Pause

        Rendering ->
            Sub.batch
                [ listenForKey "F1" (JumpForward 1)
                , listenForKey "F2" Pause
                ]


emptyAttr : E.Attribute msg
emptyAttr =
    E.htmlAttribute (Html.Attributes.classList [])


dndConfig : DnDList.Config Action
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Horizontal
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


dndSystem : DnDList.System Action Msg
dndSystem =
    DnDList.create dndConfig DnDMsg
