module Main exposing (main)

import Action exposing (Action(..))
import Browser
import Element as E exposing (Element)
import Element.Background as EBg
import Element.Border as EBo
import Element.Font as EF
import Element.Input as EI
import FeatherIcons as FI exposing (Icon)
import Html.Attributes
import List.Zipper as Zipper exposing (Zipper)
import Todo
import Zoom


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { actions : List Action
    , zoom : Int
    , currentFrame : Int
    }


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


init : () -> ( Model, Cmd Msg )
init () =
    ( { actions = Todo.actions
      , zoom = 0
      , currentFrame = 0
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "CodeAnim"
    , body =
        [ E.layout
            [ EBg.color (E.rgb255 0x33 0x33 0x33)
            , E.width E.fill
            ]
            (E.column
                [ E.centerX
                , E.width (E.fill |> E.maximum 800)
                , E.height (E.shrink |> E.minimum 500)
                , E.centerY
                , EBo.width 1
                , EBg.color (E.rgb255 0x45 0x40 0x40)
                ]
                [ viewTimeline model ]
            )
        ]
    }


viewTimeline :
    { a
        | currentFrame : Int
        , zoom : Int
        , actions : List Action
    }
    -> Element Msg
viewTimeline { currentFrame, zoom, actions } =
    let
        markerWidth : Int
        markerWidth =
            2
    in
    E.column
        [ E.width E.fill
        , E.alignBottom
        , EBo.width 1
        , E.clip
        ]
        [ E.row [] (List.map (viewAction zoom) actions)
        , E.el
            [ E.moveRight
                (toFloat
                    (Zoom.msToPx
                        { ms = round <| frameToMs currentFrame
                        , zoom = zoom
                        }
                        -- centering:
                        - (markerWidth // 2)
                    )
                )
            , E.width (E.px markerWidth)
            , E.height (E.px 20)
            , EBg.color (E.rgb255 0xCC 0xCC 0xCC)
            ]
            E.none
        , viewTimelineButtons
        ]


viewTimelineButtons : Element Msg
viewTimelineButtons =
    E.row
        [ E.spaceEvenly
        , E.width E.fill
        ]
        [ E.el []
            (E.row [ E.spacing 2 ]
                [ viewTimelineButton JumpToStart FI.cornerLeftDown "Jump to start"
                , viewTimelineButton JumpToPrevious FI.skipBack "Jump to previous action"
                , viewTimelineButton (JumpBackward 10) FI.chevronsLeft "Jump backward"
                , viewTimelineButton (JumpBackward 1) FI.chevronLeft "Step backward"
                , viewTimelineButton (JumpForward 1) FI.chevronRight "Step forward"
                , viewTimelineButton (JumpForward 10) FI.chevronsRight "Jump forward"
                , viewTimelineButton JumpToNext FI.skipForward "Jump to next action"
                , viewTimelineButton JumpToEnd FI.cornerRightDown "Jump to end"
                ]
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


timelineZoom : Float
timelineZoom =
    1 / 20


viewAction : Int -> Action -> Element Msg
viewAction zoom action =
    let
        color =
            actionBgColor action
    in
    E.el
        [ EBg.color color
        , E.height (E.px 70)
        , E.width
            (E.px
                (Zoom.msToPx
                    { ms = Action.duration action
                    , zoom = zoom
                    }
                )
            )
        , E.clip
        , EBo.width 2
        , EBo.color (darken 0.1 color)
        , EF.color (darken 0.3 color)
        ]
        (viewActionText action)


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


viewActionText : Action -> Element Msg
viewActionText action =
    E.el
        [ E.paddingEach
            { left = 5
            , top = 5
            , bottom = 0
            , right = 0
            }
        ]
        (E.text <|
            case action of
                TypeText _ ->
                    "Type"

                Wait _ ->
                    "Wait"

                FadeOut _ ->
                    "FadeOut"
        )


actionBgColor : Action -> E.Color
actionBgColor action =
    case action of
        TypeText _ ->
            E.rgb255 0xCC 0xDD 0xD0

        Wait _ ->
            E.rgb255 0xDD 0xBB 0xB0

        FadeOut _ ->
            E.rgb255 0xBB 0xCC 0xDD


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
                        model.actions
              }
            , Cmd.none
            )

        JumpToPrevious ->
            ( { model
                | currentFrame =
                    previousActionFrame
                        model.currentFrame
                        model.actions
              }
            , Cmd.none
            )

        JumpToStart ->
            ( { model | currentFrame = 0 }
            , Cmd.none
            )

        JumpToEnd ->
            ( { model | currentFrame = lastFrame model.actions }
            , Cmd.none
            )


previousActionFrame : Int -> List Action -> Int
previousActionFrame currentFrame actions =
    let
        go : Int -> List Action -> Int
        go accFrame actions_ =
            case actions_ of
                [] ->
                    accFrame

                action :: rest ->
                    let
                        durationMs =
                            Action.duration action

                        durationFrames =
                            msToFrame durationMs

                        newAccFrame =
                            accFrame + durationFrames
                    in
                    if newAccFrame >= currentFrame then
                        accFrame

                    else
                        go newAccFrame rest
    in
    go 0 actions


nextActionFrame : Int -> List Action -> Int
nextActionFrame currentFrame actions =
    let
        go : Int -> List Action -> Int
        go accFrame actions_ =
            case actions_ of
                [] ->
                    accFrame

                action :: rest ->
                    let
                        durationMs =
                            Action.duration action

                        durationFrames =
                            msToFrame durationMs

                        newAccFrame =
                            accFrame + durationFrames
                    in
                    if newAccFrame > currentFrame then
                        newAccFrame

                    else
                        go newAccFrame rest
    in
    go 0 actions


lastFrame : List Action -> Int
lastFrame actions =
    List.foldl
        (\action acc -> acc + msToFrame (Action.duration action))
        0
        actions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


frameToMs : Int -> Float
frameToMs frame =
    toFloat frame * {- 1000 ms / 60 frames -} 50 / 3


msToFrame : Int -> Int
msToFrame ms =
    round (toFloat ms / {- 1000 ms / 60 frames -} (50 / 3))
