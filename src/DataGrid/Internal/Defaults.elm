module DataGrid.Internal.Defaults exposing (..)

{-| Default styles and associated style converters.
-}

import Element
import String.Format



--------------------------------------------------------------------------------


type alias RGB =
    ( Int, Int, Int )


type alias RGBA =
    ( RGB, Float )



--------------------------------------------------------------------------------
-- Colors


rgbToString : RGB -> String
rgbToString ( r, g, b ) =
    "rgb({{r}}, {{g}}, {{b}})"
        |> String.Format.namedValue "r" (String.fromInt r)
        |> String.Format.namedValue "g" (String.fromInt g)
        |> String.Format.namedValue "b" (String.fromInt b)


rgbaToString : RGBA -> String
rgbaToString ( ( r, g, b ), a ) =
    "rgb({{r}}, {{g}}, {{b}}, {{a}})"
        |> String.Format.namedValue "r" (String.fromInt r)
        |> String.Format.namedValue "g" (String.fromInt g)
        |> String.Format.namedValue "b" (String.fromInt b)
        |> String.Format.namedValue "a" (String.fromFloat a)


rgbToElmUI : RGB -> Element.Color
rgbToElmUI ( r, g, b ) =
    Element.rgb255 r g b


rgbaToElmUI : RGBA -> Element.Color
rgbaToElmUI ( ( r, g, b ), a ) =
    Element.rgba255 r g b a


defaultTextColor : RGB
defaultTextColor =
    ( 64, 64, 64 )


defaultFillColor : RGBA
defaultFillColor =
    ( ( 52, 166, 227 ), 0.8 )


defaultHoverColor : RGBA
defaultHoverColor =
    ( ( 29, 100, 156 ), 1.0 )


defaultFillColor2 : RGBA
defaultFillColor2 =
    ( ( 118, 214, 78 ), 0.8 )


defaultHoverColor2 : RGBA
defaultHoverColor2 =
    ( ( 60, 112, 37 ), 1.0 )



--------------------------------------------------------------------------------
-- Fonts


defaultTypeface : String
defaultTypeface =
    "Consolas"
