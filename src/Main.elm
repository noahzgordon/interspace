module Main exposing (main)

import Arc2d as Arc
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events
import Coordinates exposing (Coordinates)
import Duration
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel exposing (onWheel)
import Json.Decode as Json
import Length
import LineSegment2d as Line
import List.Extra as List
import Maybe.Extra as Maybe
import Planets exposing (Planet, PlanetId(..))
import Point2d as Point
import Quantity
import Speed
import Svg exposing (Svg, circle, g, line, polygon, rect, svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Svg.Lazy exposing (lazy, lazy2, lazy3)
import Task
import Tuple exposing (first, second)


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


shipSpeed =
    Speed.kilometersPerHour 20000


minScale =
    0.005


maxScale =
    2.25


daysPassedAtStart =
    10000



{- MODEL -}


type alias Rectangle =
    { width : Float
    , height : Float
    }


type alias PlanetPositions =
    { mercury : Coordinates
    , venus : Coordinates
    , earth : Coordinates
    , mars : Coordinates
    , ceres : Coordinates
    , vesta : Coordinates
    , jupiter : Coordinates
    , saturn : Coordinates
    , uranus : Coordinates
    , neptune : Coordinates
    , pluto : Coordinates
    }


type alias Model =
    { viewport : Maybe { width : Float, height : Float }
    , scale : Float
    , focalPoint : Coordinates
    , mousePosition : Coordinates
    , dragging : Bool
    , shiftPressed : Bool
    , planetPositions : PlanetPositions
    , plottingPositions : Maybe PlanetPositions
    , playerLocation : PlanetId
    }


center =
    systemSize / 2


init : Json.Value -> ( Model, Cmd Message )
init _ =
    let
        positionPlanet planet =
            { x = center, y = center - planet.orbitalRadius }
                |> movePlanet daysPassedAtStart planet.id
    in
    ( { viewport = Nothing
      , scale = 0.01
      , focalPoint = { x = systemSize / 2, y = systemSize / 2 }
      , mousePosition = { x = systemSize / 2, y = systemSize / 2 }
      , dragging = False
      , shiftPressed = False
      , planetPositions =
            { mercury = positionPlanet Planets.mercury
            , venus = positionPlanet Planets.venus
            , earth = positionPlanet Planets.earth
            , mars = positionPlanet Planets.mars
            , ceres = positionPlanet Planets.ceres
            , vesta = positionPlanet Planets.vesta
            , jupiter = positionPlanet Planets.jupiter
            , saturn = positionPlanet Planets.saturn
            , uranus = positionPlanet Planets.uranus
            , neptune = positionPlanet Planets.neptune
            , pluto = positionPlanet Planets.pluto
            }
      , plottingPositions = Nothing
      , playerLocation = Planets.Earth
      }
    , Task.perform ReceivedViewportInfo getViewport
    )



{- UPDATE -}


type Message
    = ReceivedViewportInfo Viewport
    | ReceivedMousePosition ( Float, Float )
    | ReceivedKeyDown Key
    | ReceivedKeyUp Key
    | ScrolledMouseWheel Wheel.Event
    | MouseButtonClicked
    | MouseButtonReleased
    | PlanetClicked PlanetId
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
                            ( ((xPos + 10 - (viewport.width / 2)) / model.scale) + model.focalPoint.x
                            , ((yPos + 10 - (viewport.height / 2)) / model.scale) + model.focalPoint.y
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
                        case model.plottingPositions of
                            Just plottingPositions ->
                                let
                                    playerPosition =
                                        getPosition model.playerLocation model.planetPositions

                                    playerPoint =
                                        Point.fromCoordinates ( playerPosition.x, playerPosition.y )

                                    oldMousePoint =
                                        Point.fromCoordinates ( model.mousePosition.x, model.mousePosition.y )

                                    newMousePoint =
                                        Point.fromCoordinates ( newX, newY )

                                    oldDistance =
                                        Point.distanceFrom playerPoint oldMousePoint

                                    newDistance =
                                        Point.distanceFrom playerPoint newMousePoint

                                    daysAdjustment =
                                        ((newDistance - oldDistance) * 5000)
                                            |> Length.kilometers
                                            |> Quantity.at_ shipSpeed
                                            |> Duration.inDays
                                in
                                ( { model
                                    | mousePosition = { x = newX, y = newY }
                                    , plottingPositions =
                                        Just
                                            { mercury = movePlanet daysAdjustment Mercury plottingPositions.mercury
                                            , venus = movePlanet daysAdjustment Venus plottingPositions.venus
                                            , earth = movePlanet daysAdjustment Earth plottingPositions.earth
                                            , mars = movePlanet daysAdjustment Mars plottingPositions.mars
                                            , ceres = movePlanet daysAdjustment Ceres plottingPositions.ceres
                                            , vesta = movePlanet daysAdjustment Vesta plottingPositions.vesta
                                            , jupiter = movePlanet daysAdjustment Jupiter plottingPositions.jupiter
                                            , saturn = movePlanet daysAdjustment Saturn plottingPositions.saturn
                                            , uranus = movePlanet daysAdjustment Uranus plottingPositions.uranus
                                            , neptune = movePlanet daysAdjustment Neptune plottingPositions.neptune
                                            , pluto = movePlanet daysAdjustment Pluto plottingPositions.pluto
                                            }
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model
                                    | mousePosition = { x = newX, y = newY }
                                  }
                                , Cmd.none
                                )

                Nothing ->
                    ( model, Cmd.none )

        ReceivedKeyDown key ->
            case key of
                Shift ->
                    ( { model | shiftPressed = True }, Cmd.none )

                Escape ->
                    ( { model | plottingPositions = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReceivedKeyUp key ->
            case key of
                Shift ->
                    ( { model | shiftPressed = False }, Cmd.none )

                _ ->
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
            let
                -- 1 second = 10 days
                daysPassed =
                    delta / 100
            in
            ( { model
                | planetPositions =
                    { mercury = movePlanet daysPassed Mercury model.planetPositions.mercury
                    , venus = movePlanet daysPassed Venus model.planetPositions.venus
                    , earth = movePlanet daysPassed Earth model.planetPositions.earth
                    , mars = movePlanet daysPassed Mars model.planetPositions.mars
                    , ceres = movePlanet daysPassed Ceres model.planetPositions.ceres
                    , vesta = movePlanet daysPassed Vesta model.planetPositions.vesta
                    , jupiter = movePlanet daysPassed Jupiter model.planetPositions.jupiter
                    , saturn = movePlanet daysPassed Saturn model.planetPositions.saturn
                    , uranus = movePlanet daysPassed Uranus model.planetPositions.uranus
                    , neptune = movePlanet daysPassed Neptune model.planetPositions.neptune
                    , pluto = movePlanet daysPassed Pluto model.planetPositions.pluto
                    }
              }
            , Cmd.none
            )

        PlanetClicked planetId ->
            if planetId == model.playerLocation then
                if Maybe.isJust model.plottingPositions then
                    ( { model | plottingPositions = Nothing }, Cmd.none )

                else
                    ( { model | plottingPositions = Just model.planetPositions }, Cmd.none )

            else
                case model.plottingPositions of
                    Just positions ->
                        ( { model
                            | planetPositions = positions
                            , playerLocation = planetId
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )


movePlanet : Float -> PlanetId -> Coordinates -> Coordinates
movePlanet days planetId position =
    let
        centerPoint =
            Point.fromCoordinates ( center, center )

        positionPoint =
            Point.fromCoordinates ( position.x, position.y )

        orbitalPeriod =
            (Planets.get planetId).orbitalPeriod

        arc =
            Arc.sweptAround centerPoint (days / orbitalPeriod) positionPoint

        newPositionPoint =
            Arc.endPoint arc
    in
    { x = Point.xCoordinate newPositionPoint
    , y = Point.yCoordinate newPositionPoint
    }


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ if model.shiftPressed && Maybe.isNothing model.plottingPositions then
            Browser.Events.onAnimationFrameDelta TimePassed

          else
            Sub.none
        , Browser.Events.onKeyDown (keyDecoder |> Json.map ReceivedKeyDown)
        , Browser.Events.onKeyUp (keyDecoder |> Json.map ReceivedKeyUp)
        ]


mouseCoordinatesDecoder : Json.Decoder Coordinates
mouseCoordinatesDecoder =
    Json.map2 Coordinates (Json.field "screenX" Json.float) (Json.field "screenY" Json.float)



{- VIEW -}


view : Model -> Browser.Document Message
view model =
    { title = "INTER <> SPACE"
    , body = [ playView model ]
    }


getPosition : PlanetId -> PlanetPositions -> Coordinates
getPosition location positions =
    case location of
        Mercury ->
            positions.mercury

        Venus ->
            positions.venus

        Earth ->
            positions.earth

        Mars ->
            positions.mars

        Ceres ->
            positions.ceres

        Vesta ->
            positions.vesta

        Jupiter ->
            positions.jupiter

        Saturn ->
            positions.saturn

        Uranus ->
            positions.uranus

        Neptune ->
            positions.neptune

        Pluto ->
            positions.pluto


scaleTransform : Coordinates -> Float -> Svg.Attribute msg
scaleTransform focalPoint scale =
    transform ("translate(" ++ ((1 - scale) * focalPoint.x |> String.fromFloat) ++ "," ++ ((1 - scale) * focalPoint.y |> String.fromFloat) ++ ") scale(" ++ String.fromFloat scale ++ ")")


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

                planetList : List ( Planet, Coordinates )
                planetList =
                    case model.plottingPositions of
                        Just positions ->
                            [ ( Planets.get Mercury, positions.mercury )
                            , ( Planets.get Venus, positions.venus )
                            , ( Planets.get Earth, positions.earth )
                            , ( Planets.get Mars, positions.mars )
                            , ( Planets.get Ceres, positions.ceres )
                            , ( Planets.get Vesta, positions.vesta )
                            , ( Planets.get Jupiter, positions.jupiter )
                            , ( Planets.get Saturn, positions.saturn )
                            , ( Planets.get Uranus, positions.uranus )
                            , ( Planets.get Neptune, positions.neptune )
                            , ( Planets.get Pluto, positions.pluto )
                            ]

                        Nothing ->
                            [ ( Planets.get Mercury, model.planetPositions.mercury )
                            , ( Planets.get Venus, model.planetPositions.venus )
                            , ( Planets.get Earth, model.planetPositions.earth )
                            , ( Planets.get Mars, model.planetPositions.mars )
                            , ( Planets.get Ceres, model.planetPositions.ceres )
                            , ( Planets.get Vesta, model.planetPositions.vesta )
                            , ( Planets.get Jupiter, model.planetPositions.jupiter )
                            , ( Planets.get Saturn, model.planetPositions.saturn )
                            , ( Planets.get Uranus, model.planetPositions.uranus )
                            , ( Planets.get Neptune, model.planetPositions.neptune )
                            , ( Planets.get Pluto, model.planetPositions.pluto )
                            ]

                playerPosition =
                    getPosition model.playerLocation model.planetPositions

                planetScale =
                    clamp 0.05 maxScale model.scale
            in
            svg
                [ viewBox (String.fromFloat initX ++ " " ++ String.fromFloat initY ++ " " ++ String.fromFloat viewport.width ++ " " ++ String.fromFloat viewport.height)
                , onWheel ScrolledMouseWheel
                , Mouse.onMove (.offsetPos >> ReceivedMousePosition)
                , Mouse.onDown (always MouseButtonClicked)
                , Mouse.onUp (always MouseButtonReleased)
                , cursor
                    (if model.dragging then
                        "grabbing"

                     else
                        "grab"
                    )
                ]
                -- background elements
                [ g [ scaleTransform model.focalPoint model.scale ]
                    [ rect [ fill "black", width (String.fromFloat systemSize), height (String.fromFloat systemSize) ] []

                    -- da stars
                    , lazy3 drawStarGroup viewport model.scale model.focalPoint

                    -- da sun
                    , circle
                        [ reverseScale { x = center, y = center } 0.015 model.scale
                        , fill "yellow"
                        , r "1000"
                        , cx (String.fromFloat center)
                        , cy (String.fromFloat center)
                        ]
                        []
                    , case model.plottingPositions of
                        Just _ ->
                            line
                                [ x1 (String.fromFloat playerPosition.x)
                                , y1 (String.fromFloat playerPosition.y)
                                , x2 (String.fromFloat model.mousePosition.x)
                                , y2 (String.fromFloat model.mousePosition.y)
                                , stroke "white"
                                , strokeWidth "50"
                                , strokeDasharray "200"
                                ]
                                []

                        Nothing ->
                            Html.text ""
                    , g [] (List.map (drawPlanet model.scale model.playerLocation) planetList)
                    ]
                ]


drawStarGroup : Rectangle -> Float -> Coordinates -> Svg Message
drawStarGroup viewport scale focalPoint =
    let
        starScale =
            0.01
    in
    g
        [ transform ("translate(" ++ ((1 - starScale) * focalPoint.x |> String.fromFloat) ++ "," ++ ((1 - starScale) * focalPoint.y |> String.fromFloat) ++ ") scale(" ++ String.fromFloat starScale ++ ")") ]
        (List.map drawStar (starPositions viewport starScale focalPoint))


drawStar : ( Int, Int ) -> Svg Message
drawStar ( x, y ) =
    circle [ fill "#BBB", r "50", cx (String.fromInt x), cy (String.fromInt y) ] []


reverseScale : Coordinates -> Float -> Float -> Svg.Attribute msg
reverseScale position threshold scale =
    if scale <= threshold then
        scaleTransform position (1 / (scale / threshold))

    else
        attribute "" ""


drawPlanet : Float -> PlanetId -> ( Planet, Coordinates ) -> Svg Message
drawPlanet scale playerLocation ( planet, position ) =
    svg
        [ x (position.x - 2000 |> String.fromFloat)
        , y (position.y - 2000 |> String.fromFloat)
        , width "4000"
        , height "4000"
        , overflow "visible"
        ]
        [ g [ reverseScale { x = 2000, y = 2000 } 0.03 scale ]
            [ svg
                [ x "1700"
                , y "1100"
                , width "600"
                , height "600"
                , visibility
                    (if playerLocation == planet.id then
                        "visible"

                     else
                        "hidden"
                    )
                ]
                [ polygon
                    [ fill "red"
                    , points "210,0 210,390 90,390 300,600 510,390 390,390 390,0"
                    ]
                    []
                ]
            , circle
                [ fill planet.color
                , r "200"
                , cx "2000"
                , cy "2000"
                , cursor "pointer"
                , onClick (PlanetClicked planet.id)
                ]
                []
            ]
        ]


starPositions : Rectangle -> Float -> Coordinates -> List ( Int, Int )
starPositions viewport scale focalPoint =
    let
        xPositions =
            List.range -15 15
                |> List.map ((*) 2000000)

        yPositions =
            List.range -15 15
                |> List.map ((*) 2000000)
    in
    List.foldr
        (\xPos outerList ->
            List.foldr
                (\yPos innerList ->
                    List.append innerList [ ( xPos, yPos ) ]
                )
                outerList
                yPositions
        )
        []
        xPositions


loadingScreen : Html Message
loadingScreen =
    Html.text "loading..."


type Key
    = Shift
    | Escape
    | Other


keyDecoder : Json.Decoder Key
keyDecoder =
    Json.map toKey (Json.field "key" Json.string)


toKey : String -> Key
toKey string =
    case string of
        "Shift" ->
            Shift

        "Escape" ->
            Escape

        _ ->
            Other
