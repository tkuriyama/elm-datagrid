module DataGrid.Internal.GridChart exposing (..)

import Color exposing (Color)
import DataGrid.ChartConfig as Cfg
import DataGrid.Internal.Utils as Utils
import List.Nonempty as NE
import Scale.Color as ColorScale
import TypedSvg exposing (text_)
import TypedSvg.Attributes.InPx exposing (x, y)
import TypedSvg.Core exposing (Svg, text)



--------------------------------------------------------------------------------
-- Axes and Scales


getColor : Float -> Color
getColor f =
    if f >= 0 then
        ColorScale.viridisInterpolator (1 - f)

    else
        ColorScale.plasmaInterpolator (1 - abs f)



--------------------------------------------------------------------------------
-- Projections


projectSeries :
    List String
    -> List (Cfg.GridSeries a)
    -> List (Cfg.GridSeries a)
projectSeries hide =
    List.filter (\( name, _ ) -> List.member name hide |> not)



--------------------------------------------------------------------------------
-- Tooltips


type alias HasTooltips a =
    { a | tooltips : Cfg.Tooltips }


type alias Coord =
    ( Float, Float )


type alias HoverEnv =
    { x : Float
    , y : Float
    , h : Float
    , w : Float
    }


pairsToStrings :
    (a -> String)
    -> Int
    -> NE.Nonempty ( String, a )
    -> NE.Nonempty String
pairsToStrings fmt space pairs =
    let
        longest =
            NE.map Tuple.first pairs
                |> NE.map String.length
                |> NE.foldl1 max
    in
    NE.map (\( a, b ) -> Utils.twoCols longest space a (fmt b)) pairs


genHoverLineCoords :
    HasTooltips a
    -> NE.Nonempty String
    -> ( Float, Float )
    -> NE.Nonempty Coord
genHoverLineCoords env lines ( x0, y0 ) =
    let
        xs =
            NE.Nonempty
                x0
                (List.repeat (NE.length lines - 1) x0)

        f i =
            y0 + (i * env.tooltips.hoverTooltipSize |> toFloat) + 5

        ys =
            NE.Nonempty
                (f 1 - 5)
                (List.range 2 (NE.length lines) |> List.map f)
    in
    NE.zip xs ys


hoverDims : Float -> NE.Nonempty String -> ( Float, Float )
hoverDims sz lines =
    ( NE.map String.length lines
        |> NE.foldl1 max
        |> toFloat
        |> (\n -> n * (sz * 0.65))
    , (NE.length lines + 1 |> toFloat) * sz * 1.03
    )


renderHoverText : Float -> String -> ( Float, Float ) -> Svg msg
renderHoverText pad txt ( hx, hy ) =
    text_
        [ x <| hx + pad
        , y <| hy + pad
        ]
        [ text txt ]
