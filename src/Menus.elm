module Menus exposing (drawFor)

import Element exposing (Color, Element, alignLeft, alignRight, centerX, column, el, fill, height, layout, padding, rgb, row, shrink, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Model exposing (MarketInfo, Model)
import Planets exposing (PlanetId)


white : Color
white =
    rgb 1 1 1


drawFor : Model -> PlanetId -> Html msg
drawFor model planetId =
    let
        marketInfo : MarketInfo
        marketInfo =
            Planets.getInfo model.marketInfo planetId
    in
    layout [] <|
        column
            [ width fill
            , height fill
            ]
            [ row
                [ width fill
                , padding 20
                , Border.widthEach
                    { bottom = 1
                    , left = 0
                    , right = 0
                    , top = 0
                    }
                , Border.color white
                ]
                [ el
                    [ centerX
                    , Font.color white
                    ]
                    (text ("Market Info: " ++ Planets.name planetId))
                ]
            , row
                [ width fill
                , height fill
                , Font.color (rgb 0.8 0.8 0.8)
                ]
                [ column
                    [ width fill
                    , spacing 25
                    , Border.widthEach
                        { bottom = 0
                        , left = 0
                        , right = 1
                        , top = 0
                        }
                    , Border.color white
                    , padding 20
                    , height fill
                    ]
                  <|
                    el [ Font.underline ] (text "Buy List")
                        :: drawMarketList marketInfo.buy
                , column
                    [ width fill
                    , spacing 25
                    , padding 20
                    , height fill
                    ]
                  <|
                    el [ Font.underline ] (text "Sell List")
                        :: drawMarketList marketInfo.sell
                ]
            ]


drawMarketList : List ( String, Float ) -> List (Element msg)
drawMarketList =
    List.map
        (\( name, price ) ->
            row [ width fill ]
                [ el [ alignLeft ] (text (name ++ ": "))
                , el [ alignRight ] (text (String.fromFloat price ++ " ESD"))
                ]
        )
