module Main exposing (main)

import Tuple exposing (first, second)
import Task
import Html.Styled as Html exposing (Html)
import Html.Events.Extra.Wheel as Wheel exposing (onWheel)
import Html.Events.Extra.Mouse as Mouse
import Css exposing (cursor, pointer, grabbing)
import Svg.Styled exposing (Svg, svg, g, circle, rect)
import Svg.Styled.Attributes exposing (viewBox, fill, width, height, cx, cy, r, transform, css, fromUnstyled)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events
import Json.Decode as Json
import LineSegment2d as Line
import Point2d as Point
import Arc2d as Arc


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
    0.01


maxScale =
    2.25



{- MODEL -}


type alias Coordinates =
    { x : Float
    , y : Float
    }


type alias Planet =
    { position : Coordinates
    , color : String
    , orbitalPeriod : Float
    }


type alias Model =
    { viewport : Maybe { width : Float, height : Float }
    , scale : Float
    , focalPoint : Coordinates
    , mousePosition : Coordinates
    , dragging : Bool
    , planets : List Planet
    }


center =
    systemSize / 2


init : Json.Value -> ( Model, Cmd Message )
init _ =
    ( { viewport = Nothing
      , scale = minScale
      , focalPoint = { x = systemSize / 2, y = systemSize / 2 }
      , mousePosition = { x = systemSize / 2, y = systemSize / 2 }
      , dragging = False
      , planets =
            -- , { color = "black", position = { x = orbits.ceresBelt, y = systemSize / 2 } }
            [ { color = "#B1ADAD"
              , position = { x = center, y = center - orbits.mercury }
              , orbitalPeriod = 88
              }
            , { color = "#DE5F25"
              , position = { x = center, y = center - orbits.venus }
              , orbitalPeriod = 224.7
              }
            , { color = "#182A61"
              , position = { x = center, y = center - orbits.earth }
              , orbitalPeriod = 365.2
              }
            , { color = "#B53B03"
              , position = { x = center, y = center - orbits.mars }
              , orbitalPeriod = 687.0
              }
            , { color = "#C1844D"
              , position = { x = center, y = center - orbits.jupiter }
              , orbitalPeriod = 4331
              }
            , { color = "#C1B494"
              , position = { x = center, y = center - orbits.saturn }
              , orbitalPeriod = 10747
              }
            , { color = "#D3F9FA"
              , position = { x = center, y = center - orbits.uranus }
              , orbitalPeriod = 30589
              }
            , { color = "#3454DF"
              , position = { x = center, y = center - orbits.neptune }
              , orbitalPeriod = 59800
              }
            , { color = "#E9E8D2"
              , position = { x = center, y = center - orbits.pluto }
              , orbitalPeriod = 90560
              }
            ]
      }
    , Task.perform ReceivedViewportInfo getViewport
    )



{- UPDATE -}


type Message
    = ReceivedViewportInfo Viewport
    | ReceivedMousePosition ( Float, Float )
    | ScrolledMouseWheel Wheel.Event
    | MouseButtonClicked
    | MouseButtonReleased
    | TimePassed Float


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        ReceivedViewportInfo { viewport } ->
            ( { model
                | viewport = Just { width = viewport.width, height = viewport.height }
              }
              -- get mouse pos?
            , Cmd.none
            )

        ReceivedMousePosition ( xPos, yPos ) ->
            -- set mouse pos in terms of global coord system, not viewport
            case model.viewport of
                Just viewport ->
                    let
                        ( newX, newY ) =
                            ( ((xPos - (viewport.width / 2)) / model.scale) + model.focalPoint.x
                            , ((yPos - (viewport.height / 2)) / model.scale) + model.focalPoint.y
                            )
                    in
                        if model.dragging then
                            ( { model
                                | focalPoint =
                                    { x = model.focalPoint.x + (model.mousePosition.x - newX)
                                    , y = model.focalPoint.y + (model.mousePosition.y - newY)
                                    }
                              }
                            , Cmd.none
                            )
                        else
                            ( { model
                                | mousePosition = { x = newX, y = newY }
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        MouseButtonClicked ->
            ( { model | dragging = True }, Cmd.none )

        MouseButtonReleased ->
            ( { model | dragging = False }, Cmd.none )

        ScrolledMouseWheel { deltaY } ->
            let
                newScale =
                    model.scale - (deltaY * 0.0005 * model.scale)

                newFocalPoint =
                    if deltaY < 0 then
                        Point.interpolateFrom
                            (Point.fromCoordinates ( model.focalPoint.x, model.focalPoint.y ))
                            (Point.fromCoordinates ( model.mousePosition.x, model.mousePosition.y ))
                            (-0.0005 * deltaY)
                    else
                        Point.fromCoordinates ( model.focalPoint.x, model.focalPoint.y )
            in
                if newScale < minScale || newScale > maxScale then
                    ( model, Cmd.none )
                else
                    ( { model
                        | scale = newScale
                        , focalPoint = { x = Point.xCoordinate newFocalPoint, y = Point.yCoordinate newFocalPoint }
                      }
                    , Cmd.none
                    )

        TimePassed delta ->
            ( { model | planets = List.map (movePlanet delta) model.planets }
            , Cmd.none
            )


movePlanet : Float -> Planet -> Planet
movePlanet time planet =
    let
        centerPoint =
            Point.fromCoordinates ( center, center )

        positionPoint =
            Point.fromCoordinates ( planet.position.x, planet.position.y )

        arc =
            Arc.sweptAround centerPoint (time * 10 * (1 / planet.orbitalPeriod / 365.2)) positionPoint

        newPositionPoint =
            Arc.endPoint arc
    in
        { planet
            | position =
                { x = Point.xCoordinate newPositionPoint
                , y = Point.yCoordinate newPositionPoint
                }
        }


subscriptions : Model -> Sub Message
subscriptions model =
    Browser.Events.onAnimationFrameDelta TimePassed


mouseCoordinatesDecoder : Json.Decoder Coordinates
mouseCoordinatesDecoder =
    Json.map2 Coordinates (Json.field "screenX" Json.float) (Json.field "screenY" Json.float)



{- VIEW -}


view : Model -> Browser.Document Message
view model =
    { title = "INTER <> SPACE"
    , body = [ Html.toUnstyled (playView model) ]
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
                    , onWheel ScrolledMouseWheel |> fromUnstyled
                    , Mouse.onMove (.offsetPos >> ReceivedMousePosition) |> fromUnstyled
                    , Mouse.onDown (always MouseButtonClicked) |> fromUnstyled
                    , Mouse.onUp (always MouseButtonReleased) |> fromUnstyled
                    , css
                        [ cursor
                            (if model.dragging then
                                grabbing
                             else
                                pointer
                            )
                        ]
                    ]
                    [ g [ transform ("translate(" ++ ((1 - model.scale) * model.focalPoint.x |> String.fromFloat) ++ "," ++ ((1 - model.scale) * model.focalPoint.y |> String.fromFloat) ++ ") scale(" ++ (String.fromFloat model.scale) ++ ")") ]
                        ([ rect [ fill "black", width (String.fromFloat systemSize), height (String.fromFloat systemSize) ] []
                           -- da sun
                         , circle [ fill "yellow", r "500", cx (String.fromFloat center), cy (String.fromFloat center) ] []
                         ]
                            ++ List.map drawPlanet model.planets
                        )
                    ]


drawPlanet : Planet -> Svg Message
drawPlanet planet =
    circle [ fill planet.color, r "200", cx (String.fromFloat planet.position.x), cy (String.fromFloat planet.position.y) ] []


loadingScreen : Html Message
loadingScreen =
    Html.text "loading..."
