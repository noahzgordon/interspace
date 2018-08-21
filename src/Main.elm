module Main exposing (main)

import Task
import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (viewBox, fill, width, height, cx, cy, r)
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


type alias Model =
    { viewport : Maybe Viewport }


init : Json.Value -> ( Model, Cmd Message )
init _ =
    ( { viewport = Nothing }
    , Task.perform ReceivedViewportInfo getViewport
    )



{- UPDATE -}


type Message
    = ReceivedViewportInfo Viewport


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        ReceivedViewportInfo viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )


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
                    (viewport.scene.width / 2) - (viewport.viewport.width / 2)

                initY =
                    (viewport.scene.height / 2) - (viewport.viewport.height / 2)
            in
                svg [ viewBox ((String.fromFloat initX) ++ " " ++ (String.fromFloat initY) ++ " " ++ (String.fromFloat viewport.viewport.width) ++ " " ++ (String.fromFloat viewport.viewport.height)) ]
                    [ Svg.rect [ fill "black", width "10000", height "10000" ] []
                    , Svg.circle [ fill "yellow", r "100", cx (String.fromFloat (viewport.scene.width / 2)), cy (String.fromFloat (viewport.scene.height / 2)) ] []
                    ]


loadingScreen : Html Message
loadingScreen =
    Html.text "loading..."
