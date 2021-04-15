module DataGrid.TreeGridChart exposing (render, sumWeights)

{-| Render a a single Tree Grid Chart.
-}

import Axis
import Color exposing (Color)
import DataGrid.ChartConfig as Cfg
import DataGrid.Internal.Defaults as Defaults
import DataGrid.Internal.UI as UI
import DataGrid.Internal.SquarifiedTreemap as ST
import DataGrid.Internal.Utils as Utils
import List.Extra as LE
import List.Nonempty as NE
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Scale.Color as ColorScale
import Shape
import String.Format
import TypedSvg exposing (g, line, rect, style, svg, text_)
import TypedSvg.Attributes
    exposing
        ( class
        , fill
        , transform
        , viewBox
        )
import TypedSvg.Attributes.InPx
    exposing
        ( height
        , rx
        , strokeWidth
        , width
        , x
        , y
        )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), Transform(..))



--------------------------------------------------------------------------------


type alias ChartEnv =
    { w : Float
    , h : Float
    , pad : Cfg.Padding
    , tooltips : Cfg.Tooltips
    , innerPad : Int
    , baseFontSize : Int
    , minFontSize : Int
    , style : String
    }


genChartEnv : Cfg.GridChartCfg -> List (Cfg.GridSeries Cfg.GridTriple) -> ChartEnv
genChartEnv cfg data =
    let
        ( innerPad_, baseFontSize_, minFontSize_ ) =
            parseChartSpec cfg.chartSpec
    in
    { w = cfg.w
    , h = cfg.h
    , pad = cfg.pad
    , tooltips = cfg.tooltips
    , innerPad = innerPad_
    , baseFontSize = baseFontSize_
    , minFontSize = minFontSize_
    , style = genStyle cfg baseFontSize_
    }


parseChartSpec : Cfg.ChartSpec -> ( Int, Int, Int )
parseChartSpec spec =
    case spec of
        Cfg.TreeGridChartSpec s ->
            ( s.innerPad, s.cellBaseFontSize, s.cellMinFontSize )

        _ ->
            ( 0, 0, 0 )



--------------------------------------------------------------------------------
-- Render


type alias Group =
    { name : String
    , area : Float
    }


render : Cfg.GridChartCfg -> List (Cfg.GridSeries Cfg.GridTriple) -> Svg msg
render cfg data =
    let
        env =
            genChartEnv cfg data
    in 
    case data of
        [] ->
            svg [] []
        (z :: zs) ->
            renderNonEmpty env (NE.Nonempty z zs)


renderNonEmpty :  ChartEnv -> NE.Nonempty (Cfg.GridSeries Cfg.GridTriple) -> Svg msg
renderNonEmpty env data =
    let
        dims =
            { x = env.w - env.pad.left - env.pad.right
            , y = env.h - env.pad.top - env.pad.bottom
            }

        groupCells =
            genGroups (dims.x * dims.y) data
                |> ST.makeTreemap dims

    in
    svg [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g [ class [ "tree_group" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            ( NE.map (renderGroupCell env) groupCells |> NE.toList )
        ]

renderGroupCell : ChartEnv -> (ST.Cell a) -> Svg msg
renderGroupCell env cell =
    let
        cell_ =
            padCell 6 cell
    in
    rect
        [ x cell_.x
        , y cell_.y
        , width cell_.w
        , height cell_.h
        ]
        []

genGroups : Float -> NE.Nonempty (Cfg.GridSeries Cfg.GridTriple) -> NE.Nonempty Group
genGroups area data =
    let
        groups = 
            NE.map sumWeights data

        totalWeight =
            NE.map (.area) groups |> NE.foldl1 (+)

        scalar =
            area / totalWeight
    in
        NE.sortBy (.area >> (*) -1) groups
        |> NE.map (\s -> { s | area = s.area * scalar})


sumWeights : Cfg.GridSeries Cfg.GridTriple -> Group
sumWeights (name, pairs) =
    let

        triples =
            Utils.snds pairs
                |> List.map (LE.last >> Maybe.withDefault ("", 0, 0))

        area =
            List.map (\(_, w, _) -> w) triples
                |> LE.foldl1 (+)
                |> Maybe.withDefault 0

    in
    { name = name
    , area = area
    }

--------------------------------------------------------------------------------
-- Style


genStyle : Cfg.GridChartCfg -> Int -> String
genStyle cfg sz =
    let
        ( fCfg, tCfg ) =
            ( cfg.fontSpec, cfg.tooltips )
    in
    """
     text { font-family: {{typeface}}, monospace, sans-serif;
            fill: {{textColor}}; }
     .tree_group rect { display: inline; fill: none;
                        stroke: rgb(160, 160, 160); stroke-width: 1.5px; }
     """
        |> String.Format.namedValue "sz" (String.fromInt sz)
        |> String.Format.namedValue "textColor" fCfg.textColor
        |> String.Format.namedValue "szH" (String.fromInt tCfg.hoverTooltipSize)
        |> String.Format.namedValue "typeface" fCfg.typeface
        |> String.Format.namedValue "showHover"
            (UI.display tCfg.showHoverTooltips)


--------------------------------------------------------------------------------
-- Helpers


type alias HasXYWH a =
    { a | x : Float, y : Float, w : Float, h : Float }


padCell : Float -> HasXYWH a -> HasXYWH a
padCell px cell =
    let
        xPad =
            px / 2
        yPad =
            px / 2
    in
        { cell
            | x = cell.x + xPad
            , y = cell.y + yPad
            , w = cell.w - xPad
              , h = cell.h - yPad
        }
