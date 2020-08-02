module Main exposing (main)

import Action exposing (Action, RawAction)
import Browser
import Browser.Events
import Config exposing (fps, fpsF)
import Element as E exposing (Element)
import Element.Background as EBg
import Element.Border as EBo
import Element.Font as EF
import Element.Input as EI
import FeatherIcons as FI exposing (Icon)
import Format
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import List.Zipper as Zipper exposing (Zipper)
import Markdown
import Project exposing (Project)
import Scene exposing (Scene)
import Svg
import Svg.Attributes
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


type alias Model =
    { zoom : Int
    , currentFrame : Int
    , project : Project
    , state : State
    , leftoverDelta : Float
    , hoveringAtFrame : Maybe Int
    }


type State
    = Paused
    | Playing


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
    | HoverAt Int
    | HoverOff
    | JumpToFrameAtPx Int


init : () -> ( Model, Cmd Msg )
init () =
    ( { zoom = 0
      , currentFrame = 0
      , project = Todo.project
      , state = Paused
      , hoveringAtFrame = Nothing
      , leftoverDelta = 0
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
        (E.column
            [ E.width E.fill
            , E.height E.fill
            , E.clip
            ]
            [ viewPreview model
            , viewTimeline model
            ]
        )


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
    }
    -> Element Msg
viewActions ({ project } as model) =
    E.row
        [ E.htmlAttribute (Html.Events.on "mousemove" currentPxDecoder)
            |> E.mapAttribute HoverAt
        , E.htmlAttribute (Html.Events.on "click" currentPxDecoder)
            |> E.mapAttribute JumpToFrameAtPx
        , E.htmlAttribute (Html.Events.onMouseOut HoverOff)
        , E.width E.fill
        ]
        (List.map (viewAction model) project.actions)


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
                [ viewTimelineButton JumpToStart FI.cornerLeftDown "Jump to start"
                , viewTimelineButton JumpToPrevious FI.skipBack "Jump to previous action"
                , viewTimelineButton (JumpBackward 10) FI.chevronsLeft "Jump backward (10f)"
                , viewTimelineButton (JumpBackward 1) FI.chevronLeft "Step backward"
                , case state of
                    Paused ->
                        viewTimelineButton Play FI.play "Play"

                    Playing ->
                        viewTimelineButton Pause FI.pause "Pause"
                , viewTimelineButton (JumpForward 1) FI.chevronRight "Step forward"
                , viewTimelineButton (JumpForward 10) FI.chevronsRight "Jump forward (10f)"
                , viewTimelineButton JumpToNext FI.skipForward "Jump to next action"
                , viewTimelineButton JumpToEnd FI.cornerRightDown "Jump to end"
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
                [ viewTimelineButton ZoomOut FI.zoomOut "Zoom out"
                , viewTimelineButton ResetZoom FI.monitor "Reset zoom"
                , viewTimelineButton ZoomIn FI.zoomIn "Zoom in"
                ]
            )
        ]


viewTimelineButton : Msg -> Icon -> String -> Element Msg
viewTimelineButton msg icon tooltip =
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


viewAction : { a | zoom : Int } -> Action -> Element Msg
viewAction { zoom } action =
    let
        color =
            Action.bgColor action.raw
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
        , E.clip
        , EBo.width 2
        , EBo.color (darken 0.1 color)
        , EBo.rounded 4
        , EF.color (darken 0.3 color)
        , EF.size 14
        , E.htmlAttribute (Html.Attributes.title (Action.tooltip action.raw))
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
        [ E.paddingEach
            { left = 5
            , top = 5
            , bottom = 0
            , right = 0
            }
        ]
        (E.text (Action.label action))


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
            ( { model | currentFrame = model.currentFrame + n }
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
            ( { model | state = Playing }
            , Cmd.none
            )

        Pause ->
            ( { model | state = Paused }
            , Cmd.none
            )

        Tick delta ->
            case model.state of
                Playing ->
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

                        ( newCurrentFrame, newState ) =
                            if hasEnded then
                                ( model.project.endFrame, Paused )

                            else
                                ( advancedCurrentFrame, Playing )
                    in
                    ( { model
                        | leftoverDelta = newLeftoverDelta
                        , currentFrame = newCurrentFrame
                        , state = newState
                      }
                    , Cmd.none
                    )

                Paused ->
                    ( model, Cmd.none )

        HoverAt px ->
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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Paused ->
            Sub.none

        Playing ->
            Browser.Events.onAnimationFrameDelta Tick
