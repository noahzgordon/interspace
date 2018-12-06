module Planets exposing (Planet, PlanetId(..), PlanetInfo, ceres, earth, get, getInfo, jupiter, mars, mercury, name, neptune, pluto, saturn, uranus, venus, vesta)

import Coordinates exposing (Coordinates)


type PlanetId
    = Mercury
    | Venus
    | Earth
    | Mars
    | Ceres
    | Vesta
    | Jupiter
    | Saturn
    | Uranus
    | Neptune
    | Pluto


type alias PlanetInfo a =
    { mercury : a
    , venus : a
    , earth : a
    , mars : a
    , ceres : a
    , vesta : a
    , jupiter : a
    , saturn : a
    , uranus : a
    , neptune : a
    , pluto : a
    }


type alias Planet =
    { color : String
    , orbitalPeriod : Float
    , orbitalRadius : Float
    , id : PlanetId
    }


name : PlanetId -> String
name id =
    case id of
        Mercury ->
            "Mercury"

        Venus ->
            "Venus"

        Earth ->
            "Earth"

        Mars ->
            "Mars"

        Ceres ->
            "Ceres"

        Vesta ->
            "4 Vesta"

        Jupiter ->
            "Jupiter"

        Saturn ->
            "Saturn"

        Uranus ->
            "Uranus"

        Neptune ->
            "Neptune"

        Pluto ->
            "Pluto"


mercury : Planet
mercury =
    { color = "#B1ADAD"
    , orbitalPeriod = 88
    , orbitalRadius = 57910000 / 5000
    , id = Mercury
    }


venus : Planet
venus =
    { color = "#DE5F25"
    , orbitalPeriod = 224.7
    , orbitalRadius = 108200000 / 5000
    , id = Venus
    }


earth : Planet
earth =
    { color = "#182A61"
    , orbitalPeriod = 365.2
    , orbitalRadius = 149600000 / 5000
    , id = Earth
    }


mars : Planet
mars =
    { color = "#B53B03"
    , orbitalPeriod = 687.0
    , orbitalRadius = 227900000 / 5000
    , id = Mars
    }


jupiter : Planet
jupiter =
    { color = "#C1844D"
    , orbitalPeriod = 4331
    , orbitalRadius = 778600000 / 5000
    , id = Jupiter
    }


saturn : Planet
saturn =
    { color = "#C1B494"
    , orbitalPeriod = 10747
    , orbitalRadius = 1433000000 / 5000
    , id = Saturn
    }


uranus : Planet
uranus =
    { color = "#D3F9FA"
    , orbitalPeriod = 30589
    , orbitalRadius = 2877000000 / 5000
    , id = Uranus
    }


neptune : Planet
neptune =
    { color = "#3454DF"
    , orbitalPeriod = 59800
    , orbitalRadius = 4503000000 / 5000
    , id = Neptune
    }


pluto : Planet
pluto =
    { color = "#E9E8D2"
    , orbitalPeriod = 90560
    , orbitalRadius = 5874000000 / 5000
    , id = Pluto
    }



-- NOTE! These orbits are not circular IRL


ceres : Planet
ceres =
    { color = "#AAAAAA"
    , orbitalPeriod = 1681.63
    , orbitalRadius = 414010000 / 5000
    , id = Ceres
    }


vesta : Planet
vesta =
    { color = "#AAAAAA"
    , orbitalPeriod = 1325.75
    , orbitalRadius = 353318755 / 5000
    , id = Vesta
    }


get : PlanetId -> Planet
get id =
    case id of
        Mercury ->
            mercury

        Venus ->
            venus

        Earth ->
            earth

        Mars ->
            mars

        Ceres ->
            ceres

        Vesta ->
            vesta

        Jupiter ->
            jupiter

        Saturn ->
            saturn

        Uranus ->
            uranus

        Neptune ->
            neptune

        Pluto ->
            pluto


getInfo : PlanetInfo a -> PlanetId -> a
getInfo info id =
    case id of
        Mercury ->
            info.mercury

        Venus ->
            info.venus

        Earth ->
            info.earth

        Mars ->
            info.mars

        Ceres ->
            info.ceres

        Vesta ->
            info.vesta

        Jupiter ->
            info.jupiter

        Saturn ->
            info.saturn

        Uranus ->
            info.uranus

        Neptune ->
            info.neptune

        Pluto ->
            info.pluto
