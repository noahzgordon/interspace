module Planets exposing (Planet, PlanetId(..), ceres, earth, get, jupiter, mars, mercury, neptune, pallas, pluto, saturn, uranus, venus, vesta)

import Coordinates exposing (Coordinates)


type PlanetId
    = Mercury
    | Venus
    | Earth
    | Mars
    | Ceres
    | Pallas
    | Vesta
    | Jupiter
    | Saturn
    | Uranus
    | Neptune
    | Pluto


type alias Planet =
    { color : String
    , orbitalPeriod : Float
    , orbitalRadius : Float
    , id : PlanetId
    }


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


pallas : Planet
pallas =
    { color = "#AAAAAA"
    , orbitalPeriod = 1686
    , orbitalRadius = 414804976 / 5000
    , id = Pallas
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

        Pallas ->
            pallas

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
