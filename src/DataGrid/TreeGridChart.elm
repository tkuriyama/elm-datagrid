module DataGrid.TreeGridChart exposing (render)

{-| Render a a single Tree Grid Chart.
-}

import Axis
import Color exposing (Color)
import DataGrid.ChartConfig as Cfg
import DataGrid.Internal.Defaults as Defaults
import DataGrid.Internal.UI as UI
import DataGrid.Internal.Utils as Utils
import List.Extra as LE
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


genChartEnv : Cfg.GridChartCfg -> List (Cfg.GridSeries Cfg.GridPair) -> ChartEnv
genChartEnv cfg data =
    let
        (innerPad_, baseFontSize_, minFontSize_) =
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

parseChartSpec : Cfg.ChartSpec -> (Int, Int, Int)
parseChartSpec spec =
    case spec of
        Cfg.TreeGridChartSpec s ->
            ( s.innerPad, s.cellBaseFontSize, s.cellMinFontSize )
        _ ->
            ( 0, 0, 0 )


--------------------------------------------------------------------------------
-- Render


render : Cfg.GridChartCfg -> List (Cfg.GridSeries Cfg.GridTriple) -> Svg msg
render cfg data =
    svg [] []


--------------------------------------------------------------------------------
-- Style


genStyle : Cfg.GridChartCfg -> Int -> String
genStyle cfg sz =
    let (fCfg, tCfg) =
            (cfg.fontSpec, cfg.tooltips)
    in
    """
     text { font-family: {{typeface}}, monospace, sans-serif;
            fill: {{textColor}}; }
     """
        |> String.Format.namedValue "sz" (String.fromInt sz)
        |> String.Format.namedValue "textColor" fCfg.textColor
        |> String.Format.namedValue "szH" (String.fromInt tCfg.hoverTooltipSize)
        |> String.Format.namedValue "typeface" fCfg.typeface
        |> String.Format.namedValue "showHover"
            (UI.display tCfg.showHoverTooltips)
