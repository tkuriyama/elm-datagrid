module Internal.UI exposing ( Padding, toggle )


import Element exposing (..)
import Element.Background as Background
import Element.Border as Border

import Internal.Defaults as Defaults


--------------------------------------------------------------------------------

type alias Padding
    = { top : Int
      , right : Int
      , bottom : Int
      , left : Int
      }

--------------------------------------------------------------------------------
-- Labelled Boolean Toggle
-- See https://korban.net/elm/elm-ui-patterns/checkbox

toggle : Int -> String -> String -> Bool -> Element msg 
toggle sz on off isChecked =
    let
        w = max (String.length on) (String.length off)
            |> \l -> (toFloat l + 2) * (toFloat sz) * 0.55 |> round
        knob =
            el
                [ width <| px sz
                , height <| px sz
                , Border.rounded <| round (toFloat sz / 2)
                , Border.width 1
                , Background.color <| Element.rgb255 255 255 255
                ]
                none
    in
    el
        [ width <| px w
        , height <| px (sz + 2)
        , centerY
        , padding 2
        , Border.rounded <| round (toFloat sz / 3)
        , Border.width 0
        , Background.color <|
            if isChecked then Element.rgb255 0 255 0
            else Element.rgb255 220 220 220
        ] <|
            row [ width fill ] <|
                if isChecked then
                    [ el [ centerX ] <| text on, knob ]

                else
                    [ knob, el [ centerX ] <| text off ]


