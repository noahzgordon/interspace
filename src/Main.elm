module Main exposing (main)

import Task
import Html exposing (Html)
import Html.Events.Extra.Wheel as Wheel exposing (onWheel)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (viewBox, fill, width, height, cx, cy, r, transform)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Json.Decode as Json


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



{- MODEL -}


systemSize =
    10000


minScale =
    0.15


maxScale =
    2.25


type alias Model =
    { viewport : Maybe { width : Float, height : Float }
    , scale : Float
    , focalPoint : { x : Float, y : Float }
    }


init : Json.Value -> ( Model, Cmd Message )
init _ =
    ( { viewport = Nothing
      , scale = 1
      , focalPoint = { x = systemSize / 2, y = systemSize / 2 }
      }
    , Task.perform ReceivedViewportInfo getViewport
    )



{- UPDATE -}


type Message
    = ReceivedViewportInfo Viewport
    | ScrolledMouseWheel Wheel.Event


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        ReceivedViewportInfo { viewport } ->
            ( { model | viewport = Just { width = viewport.width, height = viewport.height } }, Cmd.none )

        ScrolledMouseWheel { deltaY } ->
            let
                newScale =
                    model.scale + (deltaY * 0.0001)
            in
                if newScale < minScale || newScale > maxScale then
                    ( model, Cmd.none )
                else
                    ( { model | scale = newScale }, Cmd.none )


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none



{- VIEW -}


view : Model -> Browser.Document Message
view model =
    { title = "INTER <> SPACE"
    , body = [ playView model ]
    }


playView : Model -> Html Message
playView model =
    case model.viewport of
        Nothing ->
            loadingScreen

        Just viewport ->
            let
                initX =
                    model.focalPoint.x - (viewport.width / 2)

                initY =
                    model.focalPoint.y - (viewport.height / 2)
            in
                svg
                    [ viewBox ((String.fromFloat initX) ++ " " ++ (String.fromFloat initY) ++ " " ++ (String.fromFloat viewport.width) ++ " " ++ (String.fromFloat viewport.height))
                    , onWheel ScrolledMouseWheel
                    ]
                    [ g [ transform ("translate(" ++ ((1 - model.scale) * model.focalPoint.x |> String.fromFloat) ++ "," ++ ((1 - model.scale) * model.focalPoint.y |> String.fromFloat) ++ ") scale(" ++ (String.fromFloat model.scale) ++ ")") ]
                        [ Svg.rect [ fill "black", width "10000", height "10000" ] []
                        , Svg.circle [ fill "yellow", r "100", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat model.focalPoint.y) ] []
                        ]
                    ]


loadingScreen : Html Message
loadingScreen =
    Html.text "loading..."
