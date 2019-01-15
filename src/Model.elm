module Model exposing (MarketInfo, Model, PlanetPositions, PlanetaryMarketInfo, Rectangle)

import Coordinates exposing (Coordinates)
import Planets exposing (PlanetId, PlanetInfo)
import Sprite exposing (Sprite)


type alias Rectangle =
    { width : Float
    , height : Float
    }


type alias MarketInfo =
    { buy : List ( String, Float )
    , sell : List ( String, Float )
    }


type alias PlanetPositions =
    PlanetInfo Coordinates


type alias PlanetaryMarketInfo =
    PlanetInfo MarketInfo


type alias PlanetSprites =
    PlanetInfo (Maybe Sprite)


type alias Model =
    { viewport : Maybe Rectangle
    , scale : Float
    , focalPoint : Coordinates
    , mousePosition : Coordinates
    , dragging : Bool
    , shiftPressed : Bool
    , planetPositions : PlanetPositions
    , plottingPositions : Maybe PlanetPositions
    , marketInfo : PlanetaryMarketInfo
    , playerLocation : PlanetId
    , selectedPlanet : Maybe PlanetId
    , planetSprites : PlanetSprites
    , gameTime : Float
    , playTime : Float
    }
