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



-- 1 px = 5,000 km


systemSize =
    6000000000 / 5000


orbits =
    { mercury = 57910000 / 5000
    , venus = 108200000 / 5000
    , earth = 149600000 / 5000
    , mars = 227900000 / 5000
    , ceresBelt = 413800000 / 5000
    , jupiter = 778600000 / 5000
    , saturn = 1433000000 / 5000
    , uranus = 2877000000 / 5000
    , neptune = 4503000000 / 5000
    , pluto = 5874000000 / 5000
    }


minScale =
    0.0015


maxScale =
    2.25



{- MODEL -}


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
                        [ Svg.rect [ fill "black", width (String.fromFloat systemSize), height (String.fromFloat systemSize) ] []
                        , Svg.circle [ fill "yellow", r "500", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat model.focalPoint.y) ] []
                        , Svg.circle [ fill "#B1ADAD", r "200", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat <| model.focalPoint.y - orbits.mercury) ] []
                        , Svg.circle [ fill "#DE5F25", r "200", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat <| model.focalPoint.y - orbits.venus) ] []
                        , Svg.circle [ fill "#182A61", r "200", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat <| model.focalPoint.y - orbits.earth) ] []
                        , Svg.circle [ fill "#B53B03", r "200", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat <| model.focalPoint.y - orbits.mars) ] []
                        , Svg.circle [ fill "#C1844D", r "200", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat <| model.focalPoint.y - orbits.jupiter) ] []
                        , Svg.circle [ fill "#C1B494", r "200", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat <| model.focalPoint.y - orbits.saturn) ] []
                        , Svg.circle [ fill "#D3F9FA", r "200", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat <| model.focalPoint.y - orbits.uranus) ] []
                        , Svg.circle [ fill "#3454DF", r "200", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat <| model.focalPoint.y - orbits.neptune) ] []
                        , Svg.circle [ fill "#E9E8D2", r "200", cx (String.fromFloat model.focalPoint.x), cy (String.fromFloat <| model.focalPoint.y - orbits.pluto) ] []
                        ]
                    ]


loadingScreen : Html Message
loadingScreen =
    Html.text "loading..."
