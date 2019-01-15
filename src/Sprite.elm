module Sprite exposing (Sprite, SpriteInfo, addTime, continuous, drawSVG)

import Svg exposing (Svg)
import Svg.Attributes exposing (viewBox, xlinkHref)


type Sprite
    = Sprite
        { info : SpriteInfo
        , time : Float
        , currentFrame : Maybe ( Int, Int )
        }


type alias SpriteInfo =
    { sheet : String
    , rows : Int
    , columns : Int
    , frameSize : ( Int, Int )
    , frameSequence : List ( Int, Int )
    , frameRate : Float
    }


addTime : Float -> Sprite -> Sprite
addTime addedTime (Sprite info) =
    Sprite
        { info | time = info.time + addedTime }


continuous : SpriteInfo -> Sprite
continuous info =
    Sprite
        { info = info
        , time = 0
        , currentFrame = List.head info.frameSequence
        }


drawSVG : Sprite -> Svg msg
drawSVG (Sprite info) =
    Svg.image
        [ viewBox (String.fromInt (Tuple.first info.info.frameSize) ++ " " ++ String.fromInt (Tuple.second info.info.frameSize) ++ " 0 0")
        , xlinkHref info.info.sheet
        ]
        []
