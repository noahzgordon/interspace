module Sprite exposing (Sprite, SpriteInfo, addTime, continuous, drawSVG)

import Array exposing (Array)
import Svg exposing (Svg)
import Svg.Attributes exposing (height, viewBox, width, xlinkHref)


type Sprite
    = Sprite
        { time : Float
        , sheet : String
        , frameSize : ( Int, Int )
        , frameSequence : Array ( Int, Int )
        , frameRate : Float
        , rows : Int
        , columns : Int
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
        { sheet = info.sheet
        , frameSize = info.frameSize
        , frameSequence = Array.fromList info.frameSequence
        , frameRate = info.frameRate
        , time = 0
        , rows = info.rows
        , columns = info.columns
        }


drawSVG : List (Svg.Attribute msg) -> Sprite -> Svg msg
drawSVG attrs (Sprite info) =
    let
        ( frameX, frameY ) =
            info.time
                / info.frameRate
                |> floor
                |> modBy (Array.length info.frameSequence)
                |> (\i -> Array.get i info.frameSequence)
                |> Maybe.withDefault ( 0, 0 )

        viewBoxMinX =
            (frameX - 1)
                * Tuple.first info.frameSize
                |> String.fromInt

        viewBoxMinY =
            (frameY - 1)
                * Tuple.second info.frameSize
                |> String.fromInt

        viewBoxWidth =
            Tuple.first info.frameSize
                |> String.fromInt

        viewBoxHeight =
            Tuple.second info.frameSize
                |> String.fromInt

        imageWidth =
            Tuple.first info.frameSize
                * info.columns
                |> String.fromInt

        imageHeight =
            Tuple.second info.frameSize
                * info.rows
                |> String.fromInt
    in
    Svg.svg
        ([ viewBox (viewBoxMinX ++ " " ++ viewBoxMinY ++ " " ++ viewBoxWidth ++ " " ++ viewBoxHeight)
         ]
            ++ attrs
        )
        [ Svg.image
            [ xlinkHref info.sheet
            , width imageWidth
            , height imageHeight
            ]
            []
        ]
