module Examples.BarChartStacked exposing (cfg, dataByGroup, dataByTape, main)

import DataGrid.BarChartStacked exposing (render)
import DataGrid.ChartConfig as Cfg
    exposing
        ( defaultBarChartStackedSpec
        , defaultStdChartCfg
        , defaultTooltips
        )
import SampleData.BarChartStackedSample as BarChartStackedSample
import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


main : Svg msg
main =
    chart "tape"


chart : String -> Svg msg
chart s =
    if s == "tape" then
        render cfg dataByTape

    else
        render cfg dataByGroup


cfg : Cfg.StdChartCfg String
cfg =
    { defaultStdChartCfg
        | chartSpec = barChartStackedSpec
        , dataAxisTicks = 5
        , showLabels = False
        , labelFormatter = identity
        , tooltips = tooltipsCfg
    }


tooltipsCfg : Cfg.Tooltips
tooltipsCfg =
    { defaultTooltips
        | showTooltips = False
        , showLargeTooltips = False
        , showHoverTooltips = True
        , hoverTooltipSize = 16
    }


barChartStackedSpec : Cfg.ChartSpec
barChartStackedSpec =
    case defaultBarChartStackedSpec of
        Cfg.BarChartStackedSpec d ->
            Cfg.BarChartStackedSpec d

        _ ->
            defaultBarChartStackedSpec



--------------------------------------------------------------------------------
-- Data


dataByTape : Cfg.StdSeriesPairs String
dataByTape =
    BarChartStackedSample.dataByTape


dataByGroup : Cfg.StdSeriesPairs String
dataByGroup =
    BarChartStackedSample.dataByGroup
