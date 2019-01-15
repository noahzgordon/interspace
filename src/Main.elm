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
import Menus
import Model exposing (..)
import Planets exposing (Planet, PlanetId(..), PlanetInfo)
import Point2d as Point
import Quantity
import Speed
import Sprite
import Svg exposing (Svg, circle, foreignObject, g, line, polygon, rect, svg, text, text_)
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


center =
    systemSize / 2


init : Json.Value -> ( Model, Cmd Message )
init _ =
    let
        positionPlanet planet =
            { x = center, y = center - planet.orbitalRadius }
                |> movePlanet daysPassedAtStart planet.id

        spriteFor planet =
            Maybe.map
                (\s ->
                    Sprite.continuous
                        { sheet = s
                        , rows = 1
                        , columns = 3
                        , frameSize = ( 64, 64 )
                        , frameRate = 500
                        , frameSequence = [ ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ]
                        }
                )
                planet.sprite
    in
    ( { viewport = Nothing
      , scale = 0.01
      , focalPoint = { x = systemSize / 2, y = systemSize / 2 }
      , mousePosition = { x = systemSize / 2, y = systemSize / 2 }
      , dragging = False
      , shiftPressed = False
      , planetPositions =
            Planets.init (\id -> positionPlanet (Planets.get id))
      , planetSprites =
            Planets.init (\id -> spriteFor (Planets.get id))
      , plottingPositions = Nothing
      , gameTime = 0
      , playTime = 0
      , marketInfo =
            { mercury =
                { buy = [], sell = [] }
            , venus =
                { buy = [], sell = [] }
            , earth =
                { buy = [], sell = [] }
            , mars =
                { buy =
                    [ ( "Water", 1092.39 )
                    , ( "Vegimax(TM) Earth Mineral Mixture", 509.29 )
                    , ( "Faunalite(TM) Livestock Starter Kit", 201.42 )
                    , ( "Toy Spaceships", 54.24 )
                    ]
                , sell =
                    [ ( "Silicon", 211.11 )
                    , ( "Iron", 423.12 )
                    , ( "Sulfur", 321.23 )
                    , ( "Martian Soil Research", 12.21 )
                    , ( "RPIF Pamphlets", 0.52 )
                    ]
                }
            , ceres =
                { buy = [], sell = [] }
            , vesta =
                { buy = [], sell = [] }
            , jupiter =
                { buy = [], sell = [] }
            , saturn =
                { buy = [], sell = [] }
            , uranus =
                { buy = [], sell = [] }
            , neptune =
                { buy = [], sell = [] }
            , pluto =
                { buy = [], sell = [] }
            }
      , playerLocation = Planets.Earth
      , selectedPlanet = Nothing
      }
    , Task.perform ReceivedViewportInfo getViewport
    )



{- UPDATE -}


type ClickTarget
    = Background


type Message
    = ReceivedViewportInfo Viewport
    | ReceivedMousePosition ( Float, Float )
    | ReceivedKeyDown Key
    | ReceivedKeyUp Key
    | ScrolledMouseWheel Wheel.Event
    | MouseButtonClicked ClickTarget
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
                                        Just <|
                                            Planets.apply (\id info -> movePlanet daysAdjustment id info) plottingPositions
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
                    ( { model
                        | plottingPositions = Nothing
                        , selectedPlanet = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ReceivedKeyUp key ->
            case key of
                Shift ->
                    ( { model | shiftPressed = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseButtonClicked target ->
            case target of
                Background ->
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
                -- 1 second = 100 days
                daysPassed =
                    delta / 10

                modelWithUpdates =
                    { model
                        | playTime = model.playTime + delta
                        , planetSprites = Planets.apply (\_ sprite -> Maybe.map (Sprite.addTime delta) sprite) model.planetSprites
                    }
            in
            if model.shiftPressed && Maybe.isNothing model.plottingPositions then
                ( { modelWithUpdates
                    | planetPositions =
                        Planets.apply (\id info -> movePlanet daysPassed id info)
                            model.planetPositions
                    , gameTime = model.gameTime + delta
                  }
                , Cmd.none
                )

            else
                ( modelWithUpdates, Cmd.none )

        PlanetClicked planetId ->
            if planetId == model.playerLocation then
                if Maybe.isJust model.plottingPositions then
                    ( { model | plottingPositions = Nothing }, Cmd.none )

                else
                    ( { model
                        | plottingPositions = Just model.planetPositions
                      }
                    , Cmd.none
                    )

            else
                case model.plottingPositions of
                    Just positions ->
                        ( { model
                            | planetPositions = positions
                            , playerLocation = planetId
                            , plottingPositions = Nothing
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( { model | selectedPlanet = Just planetId }, Cmd.none )


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
        [ Browser.Events.onAnimationFrameDelta TimePassed
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
                [ class "play-view"
                , viewBox (String.fromFloat initX ++ " " ++ String.fromFloat initY ++ " " ++ String.fromFloat viewport.width ++ " " ++ String.fromFloat viewport.height)
                , width (viewport.width + 10 |> String.fromFloat)
                , height (viewport.height + 10 |> String.fromFloat)
                , onWheel ScrolledMouseWheel
                , Mouse.onMove (.offsetPos >> ReceivedMousePosition)
                , cursor
                    (if model.dragging then
                        "grabbing"

                     else
                        "grab"
                    )
                ]
                -- background elements
                [ g
                    [ scaleTransform model.focalPoint model.scale
                    , Mouse.onDown (always (MouseButtonClicked Background))
                    , Mouse.onUp (always MouseButtonReleased)
                    ]
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
                    , g [] (List.map (drawPlanet model) planetList)
                    ]
                , drawMenu model viewport
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


drawPlanet : Model -> ( Planet, Coordinates ) -> Svg Message
drawPlanet model ( planet, position ) =
    svg
        [ x (position.x - 2000 |> String.fromFloat)
        , y (position.y - 2000 |> String.fromFloat)
        , width "4000"
        , height "4000"
        , overflow "visible"
        , class "planet-wrapper"
        ]
        [ g [ reverseScale { x = 2000, y = 2000 } 0.03 model.scale ]
            [ svg
                [ x "1700"
                , y "1100"
                , width "600"
                , height "600"
                , visibility
                    (if model.playerLocation == planet.id then
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
            , case Planets.getInfo model.planetSprites planet.id of
                Nothing ->
                    circle
                        [ fill planet.color
                        , r "200"
                        , cx "2000"
                        , cy "2000"
                        , cursor "pointer"

                        -- unify w/ MouseButtonClicked ?
                        , onClick (PlanetClicked planet.id)
                        ]
                        []

                Just sprite ->
                    svg [ x "1800", y "1800", width "400", height "400" ]
                        [ Sprite.drawSVG sprite
                        ]
            , text_
                [ class "planet-label"
                , textLength "2000"
                , x "1000"
                , y "1500"
                , fill "white"
                ]
                [ text (Planets.name planet.id) ]
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


drawMenu : Model -> Rectangle -> Svg Message
drawMenu model viewport =
    case model.selectedPlanet of
        Just planetId ->
            let
                menuX =
                    model.focalPoint.x - (viewport.width / 3)

                menuY =
                    model.focalPoint.y - (viewport.height / 3)

                -- planet =
                -- Planets.get planetId
            in
            svg
                [ width (viewport.width / 3 * 2 |> String.fromFloat)
                , height (viewport.height / 3 * 2 |> String.fromFloat)
                , x (String.fromFloat menuX)
                , y (String.fromFloat menuY)
                ]
                [ rect
                    [ width "100%"
                    , height "100%"
                    , rx "15"
                    , ry "15"
                    , fill "#444"
                    , cursor "default"
                    ]
                    []
                , foreignObject [ x "0", y "0", width "100%", height "100%" ]
                    [ Menus.drawFor model planetId ]
                ]

        Nothing ->
            text ""


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
