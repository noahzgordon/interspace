module Main exposing (main)

import Arc2d as Arc
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events
import Coordinates exposing (Coordinates)
import Css exposing (cursor, grab, grabbing, pointer)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel exposing (onWheel)
import Html.Styled as Html exposing (Html)
import Json.Decode as Json
import LineSegment2d as Line
import List.Extra as List
import Planets exposing (Planet, PlanetId(..))
import Point2d as Point
import Svg.Styled exposing (Svg, circle, g, line, polygon, rect, svg)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events exposing (onClick)
import Svg.Styled.Lazy exposing (lazy, lazy2, lazy3)
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


minScale =
    0.005


maxScale =
    2.25



{- MODEL -}


type alias Rectangle =
    { width : Float
    , height : Float
    }


type alias Model =
    { viewport : Maybe { width : Float, height : Float }
    , scale : Float
    , focalPoint : Coordinates
    , mousePosition : Coordinates
    , dragging : Bool
    , plotting : Bool
    , shiftPressed : Bool
    , planetPositions :
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
    , playerLocation : PlanetId
    }


center =
    systemSize / 2


init : Json.Value -> ( Model, Cmd Message )
init _ =
    let
        positionPlanet planet =
            { x = center, y = center - planet.orbitalRadius }
    in
    ( { viewport = Nothing
      , scale = 0.01
      , focalPoint = { x = systemSize / 2, y = systemSize / 2 }
      , mousePosition = { x = systemSize / 2, y = systemSize / 2 }
      , dragging = False
      , plotting = False
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

        ReceivedKeyDown key ->
            case key of
                Shift ->
                    ( { model | shiftPressed = True }, Cmd.none )

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
            ( { model
                | planetPositions =
                    { mercury = movePlanet delta Mercury model.planetPositions.mercury
                    , venus = movePlanet delta Venus model.planetPositions.venus
                    , earth = movePlanet delta Earth model.planetPositions.earth
                    , mars = movePlanet delta Mars model.planetPositions.mars
                    , ceres = movePlanet delta Ceres model.planetPositions.ceres
                    , vesta = movePlanet delta Vesta model.planetPositions.vesta
                    , jupiter = movePlanet delta Jupiter model.planetPositions.jupiter
                    , saturn = movePlanet delta Saturn model.planetPositions.saturn
                    , uranus = movePlanet delta Uranus model.planetPositions.uranus
                    , neptune = movePlanet delta Neptune model.planetPositions.neptune
                    , pluto = movePlanet delta Pluto model.planetPositions.pluto
                    }
              }
            , Cmd.none
            )

        PlanetClicked planetId ->
            if planetId == model.playerLocation then
                ( { model | plotting = True }, Cmd.none )

            else
                ( model, Cmd.none )


movePlanet : Float -> PlanetId -> Coordinates -> Coordinates
movePlanet time planetId position =
    let
        centerPoint =
            Point.fromCoordinates ( center, center )

        positionPoint =
            Point.fromCoordinates ( position.x, position.y )

        orbitalPeriod =
            (Planets.get planetId).orbitalPeriod

        arc =
            Arc.sweptAround centerPoint (time * 10 * (1 / orbitalPeriod / 365.2)) positionPoint

        newPositionPoint =
            Arc.endPoint arc
    in
    { x = Point.xCoordinate newPositionPoint
    , y = Point.yCoordinate newPositionPoint
    }


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ if model.shiftPressed then
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

                planetList : List ( Planet, Coordinates )
                planetList =
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
                    case model.playerLocation of
                        Mercury ->
                            model.planetPositions.mercury

                        Venus ->
                            model.planetPositions.venus

                        Earth ->
                            model.planetPositions.earth

                        Mars ->
                            model.planetPositions.mars

                        Ceres ->
                            model.planetPositions.ceres

                        Vesta ->
                            model.planetPositions.vesta

                        Jupiter ->
                            model.planetPositions.jupiter

                        Saturn ->
                            model.planetPositions.saturn

                        Uranus ->
                            model.planetPositions.uranus

                        Neptune ->
                            model.planetPositions.neptune

                        Pluto ->
                            model.planetPositions.pluto
            in
            svg
                [ viewBox (String.fromFloat initX ++ " " ++ String.fromFloat initY ++ " " ++ String.fromFloat viewport.width ++ " " ++ String.fromFloat viewport.height)
                , onWheel ScrolledMouseWheel |> fromUnstyled
                , Mouse.onMove (.offsetPos >> ReceivedMousePosition) |> fromUnstyled
                , Mouse.onDown (always MouseButtonClicked) |> fromUnstyled
                , Mouse.onUp (always MouseButtonReleased) |> fromUnstyled
                , css
                    [ Css.cursor
                        (if model.dragging then
                            grabbing

                         else
                            grab
                        )
                    ]
                ]
                [ g [ transform ("translate(" ++ ((1 - model.scale) * model.focalPoint.x |> String.fromFloat) ++ "," ++ ((1 - model.scale) * model.focalPoint.y |> String.fromFloat) ++ ") scale(" ++ String.fromFloat model.scale ++ ")") ]
                    [ rect [ fill "black", width (String.fromFloat systemSize), height (String.fromFloat systemSize) ] []

                    -- da stars
                    , lazy3 drawStarGroup viewport model.scale model.focalPoint

                    -- da sun
                    , circle [ fill "yellow", r "1000", cx (String.fromFloat center), cy (String.fromFloat center) ] []
                    , svg [ x (playerPosition.x - 300 |> String.fromFloat), y (playerPosition.y - 900 |> String.fromFloat), width "600", height "600" ]
                        [ polygon [ fill "red", points "210,0 210,390 90,390 300,600 510,390 390,390 390,0" ] []
                        ]
                    , if model.plotting then
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

                      else
                        Html.text ""
                    , g [] (List.map (drawPlanet model.playerLocation) planetList)
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
    circle [ fill "white", r "500", cx (String.fromInt x), cy (String.fromInt y) ] []


drawPlanet : PlanetId -> ( Planet, Coordinates ) -> Svg Message
drawPlanet playerLocation ( planet, position ) =
    circle
        [ fill planet.color
        , r "200"
        , cx (String.fromFloat position.x)
        , cy (String.fromFloat position.y)
        , css
            [ Css.cursor
                (if playerLocation == planet.id then
                    pointer

                 else
                    grab
                )
            ]
        , onClick (PlanetClicked planet.id)
        ]
        []


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
    | Other


keyDecoder : Json.Decoder Key
keyDecoder =
    Json.map toKey (Json.field "key" Json.string)


toKey : String -> Key
toKey string =
    case string of
        "Shift" ->
            Shift

        _ ->
            Other
