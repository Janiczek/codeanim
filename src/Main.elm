port module Main exposing (main)

import Action exposing (Action, RawAction(..))
import Browser
import Browser.Events
import Config exposing (fps, fpsF)
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
import Json.Decode exposing (Decoder)
import List.Extra
import List.Zipper as Zipper exposing (Zipper)
import Markdown
import Process
import Project exposing (Project)
import Scene exposing (Scene)
import Svg
import Svg.Attributes
import Task
import Time
import Todo
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


type alias Model =
    { zoom : Int

    -- TODO , currentScene : Scene -> use `Scene.advance` later when possible
    , currentFrame : Int
    , project : Project
    , state : State
    , leftoverDelta : Float
    , hoveringAtFrame : Maybe Int
    , modal : Maybe Modal
    }


type Modal
    = EditTypeText
        { index : Int
        , text : String
        , duration : String
        }
    | EditWait
        { index : Int
        , duration : String
        }
    | EditFadeOut
        { index : Int
        , duration : String
        }
    | EditSetText
        { index : Int
        , text : String
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
    | OpenModal Modal
    | CloseModal
    | SetModalText String
    | SetModalDuration String
    | SaveModal
    | NoOp


init : () -> ( Model, Cmd Msg )
init () =
    ( { zoom = 0
      , currentFrame = 0
      , project = Todo.project
      , state = Paused
      , hoveringAtFrame = Nothing
      , leftoverDelta = 0
      , modal = Nothing
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    E.layout
        [ EBg.color (E.rgb255 0x70 0x70 0x70)
        , E.width E.fill
        , E.height E.fill
        , EE.onClick CloseModal
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
        , E.inFront <|
            case model.modal of
                Nothing ->
                    E.none

                Just modal ->
                    viewModal modal
        ]
        [ viewPreview model
        , viewTimeline model
        ]


viewModal : Modal -> Element Msg
viewModal modal =
    E.el
        [ E.centerX
        , E.centerY
        , E.htmlAttribute
            (Html.Events.stopPropagationOn "click"
                (Json.Decode.succeed ( NoOp, True ))
            )
        ]
        (E.column
            [ E.width (E.px 600)
            , E.height (E.px 600)
            , E.padding 20
            , EBg.color (E.rgb255 0x90 0x90 0x90)
            , E.spacing 20
            ]
            (viewModalContents modal
                ++ [ E.row
                        [ E.spaceEvenly
                        , E.width E.fill
                        ]
                        [ EI.button
                            [ E.padding 20
                            , EBg.color (E.rgb255 0x90 0xB0 0x90)
                            , EBo.width 2
                            , EBo.color (E.rgb255 0x70 0x90 0x70)
                            ]
                            { onPress = Just SaveModal
                            , label =
                                E.el
                                    [ E.centerX
                                    , E.centerY
                                    ]
                                    (E.text "Save")
                            }
                        , EI.button
                            [ E.padding 20
                            , EBg.color (E.rgb255 0xB0 0x90 0x90)
                            , EBo.width 2
                            , EBo.color (E.rgb255 0x90 0x70 0x70)
                            ]
                            { onPress = Just CloseModal
                            , label =
                                E.el
                                    [ E.centerX
                                    , E.centerY
                                    ]
                                    (E.text "Cancel")
                            }
                        ]
                   ]
            )
        )


viewModalContents : Modal -> List (Element Msg)
viewModalContents modal =
    case modal of
        EditTypeText { index, text, duration } ->
            [ viewModalHeading "Type Text"
            , EI.multiline
                []
                { onChange = SetModalText
                , text = text
                , placeholder = Nothing
                , spellcheck = False
                , label =
                    EI.labelAbove []
                        (E.text "Text")
                }
            , EI.text
                []
                { onChange = SetModalDuration
                , text = duration
                , placeholder = Nothing
                , label =
                    EI.labelAbove []
                        (E.text "Duration (ms)")
                }
            ]

        EditWait { index, duration } ->
            [ viewModalHeading "Wait"
            , EI.text
                []
                { onChange = SetModalDuration
                , text = duration
                , placeholder = Nothing
                , label =
                    EI.labelAbove []
                        (E.text "Duration (ms)")
                }
            ]

        EditFadeOut { index, duration } ->
            [ viewModalHeading "Fade Out"
            , EI.text
                []
                { onChange = SetModalDuration
                , text = duration
                , placeholder = Nothing
                , label =
                    EI.labelAbove []
                        (E.text "Duration (ms)")
                }
            ]

        EditSetText { index, text } ->
            [ viewModalHeading "Set Text"
            , EI.multiline
                []
                { onChange = SetModalText
                , text = text
                , placeholder = Nothing
                , spellcheck = False
                , label =
                    EI.labelAbove []
                        (E.text "Text")
                }
            ]


viewModalHeading : String -> Element Msg
viewModalHeading heading =
    E.el
        [ EF.size 30
        , EF.bold
        ]
        (E.text heading)


viewPreview :
    { a
        | currentFrame : Int
        , project : Project
        , hoveringAtFrame : Maybe Int
    }
    -> Element Msg
viewPreview ({ project, currentFrame, hoveringAtFrame } as model) =
    let
        frame : Int
        frame =
            hoveringAtFrame
                |> Maybe.map (min project.totalFrames)
                |> Maybe.withDefault currentFrame
    in
    E.el
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
        [ EBg.color project.codeBg
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
    }
    -> Element Msg
viewTimeline ({ currentFrame, zoom, project, state, hoveringAtFrame } as model) =
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
        ]
        [ E.column
            [ E.inFront <|
                case hoveringAtFrame of
                    Just frame ->
                        viewFrameMarker
                            model
                            frame
                            (E.rgb255 0xFF 0xFF 0xFF)

                    Nothing ->
                        E.none
            , E.inFront <|
                viewFrameMarker
                    model
                    currentFrame
                    (E.rgb255 0xFF 0x00 0x00)
            , E.width E.fill
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
    }
    -> Element Msg
viewActions ({ project } as model) =
    E.row
        [ E.htmlAttribute (Html.Events.on "mousemove" currentPxDecoder)
            |> E.mapAttribute HoverAtPx
        , E.htmlAttribute (Html.Events.on "click" currentPxDecoder)
            |> E.mapAttribute JumpToFrameAtPx
        , E.htmlAttribute (Html.Events.onMouseOut HoverOff)
        , E.width E.fill
        ]
        (List.indexedMap (viewAction model) project.actions)


viewFrameMarker :
    { a | zoom : Int }
    -> Int
    -> E.Color
    -> Element Msg
viewFrameMarker { zoom } frame color =
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
                        { durationFrames = Time.ms 1000 }
                    )
                )
                FI.watch
                "Add 'Wait' action"
            , viewButton
                (AddAction
                    (FadeOutAndBlank
                        { durationFrames = Time.ms 1000 }
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
    }
    -> Int
    -> Action
    -> Element Msg
viewAction { zoom, hoveringAtFrame, project } index action =
    let
        color =
            Action.bgColor action.raw

        hoveringThisAction : Bool
        hoveringThisAction =
            hoveringAtFrame
                |> Maybe.andThen (actionIndexForFrame project)
                |> Maybe.map ((==) index)
                |> Maybe.withDefault False

        modal : Maybe Modal
        modal =
            case action.raw of
                TypeText { text, durationFrames } ->
                    Just <|
                        EditTypeText
                            { index = index
                            , text = text
                            , duration = String.fromInt (round (Time.frameToMs durationFrames))
                            }

                Wait { durationFrames } ->
                    Just <|
                        EditWait
                            { index = index
                            , duration = String.fromInt (round (Time.frameToMs durationFrames))
                            }

                FadeOutAndBlank { durationFrames } ->
                    Just <|
                        EditFadeOut
                            { index = index
                            , duration = String.fromInt (round (Time.frameToMs durationFrames))
                            }

                BlankText ->
                    Nothing

                SetText { text } ->
                    Just <|
                        EditSetText
                            { index = index
                            , text = text
                            }
    in
    E.el
        [ EBg.color color
        , E.height (E.px 70)
        , E.width
            (E.px
                (Zoom.frameToPx
                    { frame = Action.durationFrames action.raw
                    , zoom = zoom
                    }
                )
            )
        , EBo.width 2
        , EBo.color (darken 0.1 color)
        , EBo.rounded 4
        , EF.color (darken 0.3 color)
        , EF.size 14
        , case modal of
            Nothing ->
                emptyAttr

            Just modal_ ->
                EE.onDoubleClick (OpenModal modal_)
        , E.htmlAttribute (Html.Attributes.title (Action.tooltip action.raw))
        , E.inFront
            (if hoveringThisAction then
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
        ]
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
            ( { model | currentFrame = model.project.endFrame }
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
                                model.project.endFrame

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
                        { px = px
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
                        { px = px
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
                    model.project
                        |> Project.withActions (rawActions ++ [ rawAction ])
            in
            ( { model | project = newProject }
            , Cmd.none
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
                    model.project
                        |> Project.withActions newRawActions
            in
            ( { model | project = newProject }
            , Cmd.none
            )

        OpenModal modal ->
            ( { model | modal = Just modal }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modal = Nothing }
            , Cmd.none
            )

        SetModalText text ->
            ( { model
                | modal =
                    model.modal
                        |> Maybe.map (\modal -> setModalText text modal)
              }
            , Cmd.none
            )

        SetModalDuration duration ->
            ( { model
                | modal =
                    model.modal
                        |> Maybe.map (\modal -> setModalDuration duration modal)
              }
            , Cmd.none
            )

        SaveModal ->
            case model.modal of
                Nothing ->
                    ( model, Cmd.none )

                Just (EditTypeText r) ->
                    String.toInt r.duration
                        |> Maybe.map
                            (\durationMs ->
                                let
                                    duration =
                                        Time.ms durationMs

                                    newAction =
                                        TypeText
                                            { text = r.text
                                            , durationFrames = duration
                                            , position = Nothing
                                            }

                                    newActions =
                                        model.project.actions
                                            |> List.map .raw
                                            |> List.Extra.setAt r.index newAction

                                    newProject =
                                        model.project
                                            |> Project.withActions newActions
                                in
                                ( { model
                                    | modal = Nothing
                                    , project = newProject
                                  }
                                , Cmd.none
                                )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

                Just (EditWait r) ->
                    String.toInt r.duration
                        |> Maybe.map
                            (\durationMs ->
                                let
                                    duration =
                                        Time.ms durationMs

                                    newAction =
                                        Wait
                                            { durationFrames = duration }

                                    newActions =
                                        model.project.actions
                                            |> List.map .raw
                                            |> List.Extra.setAt r.index newAction

                                    newProject =
                                        model.project
                                            |> Project.withActions newActions
                                in
                                ( { model
                                    | modal = Nothing
                                    , project = newProject
                                  }
                                , Cmd.none
                                )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

                Just (EditFadeOut r) ->
                    String.toInt r.duration
                        |> Maybe.map
                            (\durationMs ->
                                let
                                    duration =
                                        Time.ms durationMs

                                    newAction =
                                        FadeOutAndBlank
                                            { durationFrames = duration }

                                    newActions =
                                        model.project.actions
                                            |> List.map .raw
                                            |> List.Extra.setAt r.index newAction

                                    newProject =
                                        model.project
                                            |> Project.withActions newActions
                                in
                                ( { model
                                    | modal = Nothing
                                    , project = newProject
                                  }
                                , Cmd.none
                                )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

                Just (EditSetText r) ->
                    let
                        newAction =
                            SetText
                                { text = r.text }

                        newActions =
                            model.project.actions
                                |> List.map .raw
                                |> List.Extra.setAt r.index newAction

                        newProject =
                            model.project
                                |> Project.withActions newActions
                    in
                    ( { model
                        | modal = Nothing
                        , project = newProject
                      }
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )


setModalText : String -> Modal -> Modal
setModalText text modal =
    case modal of
        EditTypeText r ->
            EditTypeText
                { r | text = text }

        EditSetText r ->
            EditSetText
                { r | text = text }

        EditWait _ ->
            modal

        EditFadeOut _ ->
            modal


setModalDuration : String -> Modal -> Modal
setModalDuration duration modal =
    case modal of
        EditTypeText r ->
            EditTypeText
                { r | duration = duration }

        EditSetText _ ->
            modal

        EditWait r ->
            EditWait
                { r | duration = duration }

        EditFadeOut r ->
            EditFadeOut
                { r | duration = duration }


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


onKeyPress : String -> Msg -> Decoder Msg
onKeyPress key msg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                if string == key then
                    Json.Decode.succeed msg

                else
                    Json.Decode.fail "don't care"
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        listenForTick () =
            Browser.Events.onAnimationFrameDelta Tick

        listenForKey key msg =
            Browser.Events.onKeyPress (onKeyPress key msg)
    in
    case model.state of
        Paused ->
            Sub.batch
                [ if model.modal == Nothing then
                    Sub.batch
                        [ listenForKey " " Play
                        , listenForKey "r" StartRendering
                        ]

                  else
                    Sub.none
                ]

        Playing ->
            Sub.batch
                [ listenForTick ()
                , if model.modal == Nothing then
                    Sub.batch
                        [ listenForKey " " Play
                        , listenForKey "r" StartRendering
                        ]

                  else
                    Sub.none
                ]

        StartingFullscreen ->
            listenForKey " " Pause

        PlayingFullscreen ->
            Sub.batch
                [ listenForTick ()
                , listenForKey " " Pause
                ]

        EndingFullscreen ->
            listenForKey " " Pause

        Rendering ->
            listenForKey " " (JumpForward 1)


emptyAttr : E.Attribute msg
emptyAttr =
    E.htmlAttribute (Html.Attributes.classList [])
