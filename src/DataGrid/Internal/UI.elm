module DataGrid.Internal.UI exposing (..)

{-| Elm UI components and helpers.
-}

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes



--------------------------------------------------------------------------------
-- Labelled Boolean Toggle
-- See https://korban.net/elm/elm-ui-patterns/checkbox


toggle : Int -> String -> String -> Bool -> Element msg
toggle sz on off isChecked =
    let
        w =
            max (String.length on) (String.length off)
                |> (\l -> (toFloat l + 2) * toFloat sz * 0.55 |> round)

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
            if isChecked then
                Element.rgb255 0 255 0

            else
                Element.rgb255 220 220 220
        ]
    <|
        row [ width fill ] <|
            if isChecked then
                [ el [ centerX ] <| text on, knob ]

            else
                [ knob, el [ centerX ] <| text off ]



--------------------------------------------------------------------------------
-- Links


genLinks : List ( String, String ) -> Element msg
genLinks links =
    let
        f ( desc, url ) =
            link
                [ Font.underline, Font.color <| Element.rgb255 0 0 255 ]
                { url = url, label = text desc }

        linkElems =
            List.map f links |> List.intersperse (text " | ")
    in
    paragraph
        []
        (if List.length linkElems == 0 then
            [ Element.none ]

         else
            text "[" :: linkElems ++ [ text "]" ]
        )



--------------------------------------------------------------------------------
-- Padding


type alias Padding =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


zeroPad : Padding
zeroPad =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


padRight : Int -> Element.Attribute msg
padRight n =
    paddingEach { zeroPad | right = n }


padLeft : Int -> Element.Attribute msg
padLeft n =
    paddingEach { zeroPad | left = n }


padBottom : Int -> Element.Attribute msg
padBottom n =
    paddingEach { zeroPad | bottom = n }



--------------------------------------------------------------------------------
-- Tooptip


htmlTooltip : String -> Element.Attribute msg
htmlTooltip s =
    htmlAttribute <| Html.Attributes.title s



--------------------------------------------------------------------------------
-- Styling


maybeLength : Maybe Int -> Length -> Length
maybeLength mn default =
    case mn of
        Just n ->
            fill |> maximum n

        Nothing ->
            default


display : Bool -> String
display b =
    if b then
        "inline"

    else
        "none"


reveal : Bool -> Float -> String
reveal b n =
    if b then
        String.fromFloat n

    else
        "0.0"
