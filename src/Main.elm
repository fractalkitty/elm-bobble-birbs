module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr exposing (class)
import Html.Events exposing (onMouseEnter, onMouseLeave, on)
import Json.Decode as Decode
import Svg exposing (svg, g, circle, polyline, path)
import Svg.Attributes as SvgAttr exposing (cx, cy, r, d, fill, points, viewBox, style, class)
import Task exposing (Task)
import Process exposing (sleep)
import Platform.Cmd exposing (Cmd)

type alias Model =
    { birds : List Bird }

type alias Bird =
    { id : Int
    , isAnimating : Bool
    }

init : () -> (Model, Cmd Msg)
init _ =
    ({ birds = List.range 1 1000 |> List.map (\id -> Bird id False) }, Cmd.none)

type Msg
    = StartAnimation Int
    | StopAnimation Int
    | DelayedStopAnimation Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StartAnimation id ->
            ({ model | birds = List.map (updateBird id True) model.birds }, Cmd.none)

        StopAnimation id ->
            let
                task =
                    Task.perform (\_ -> DelayedStopAnimation id) (sleep 5000)
            in
            (model, task)

        DelayedStopAnimation id ->
            ({ model | birds = List.map (updateBird id False) model.birds }, Cmd.none)

updateBird : Int -> Bool -> Bird -> Bird
updateBird id animating bird =
    if bird.id == id then
        { bird | isAnimating = animating }
    else
        bird

view : Model -> Html Msg
view model =
    div [ HtmlAttr.class "grid-container" ]
        (List.map viewBird model.birds)

viewBird : Bird -> Html Msg
viewBird bird =
    div [ HtmlAttr.class "birb-wrapper"
        , onMouseEnter (StartAnimation bird.id)
        , onMouseLeave (StopAnimation bird.id)
        , on "touchstart" (Decode.succeed (StartAnimation bird.id))
        , on "touchend" (Decode.succeed (StopAnimation bird.id))
        ]
        [ svg [ SvgAttr.class "birb", SvgAttr.viewBox "0 0 120 120" ]
            [ circle [ SvgAttr.cx "60", SvgAttr.cy "60", SvgAttr.r "30" ] [] -- Body
            , g [ SvgAttr.class "birb-head", SvgAttr.style ("animation-play-state: " ++ if bird.isAnimating then "running" else "paused") ]
                [ circle [ SvgAttr.cx "60", SvgAttr.cy "30", SvgAttr.r "15" ] [] -- Head
                , circle [ SvgAttr.cx "50", SvgAttr.cy "25", SvgAttr.r "5", SvgAttr.fill "white" ] [] -- Left Eye
                , circle [ SvgAttr.cx "70", SvgAttr.cy "25", SvgAttr.r "5", SvgAttr.fill "white" ] [] -- Right Eye
                , circle [ SvgAttr.cx "50", SvgAttr.cy "25", SvgAttr.r "1", SvgAttr.fill "black" ] [] -- Left Pupil
                , circle [ SvgAttr.cx "70", SvgAttr.cy "25", SvgAttr.r "1", SvgAttr.fill "black" ] [] -- Right Pupil
                , path [ SvgAttr.d "M 60 30 Q 50 40 60 50 Q 70 40 60 30 Z", SvgAttr.fill "white" ] [] -- Beak
                ]
            ]
        ]

main =
    Browser.element { init = init, update = update, subscriptions = \_ -> Sub.none, view = view }
