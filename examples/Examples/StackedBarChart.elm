module Examples.StackedBarChart exposing (cfg, dataByGroup, dataByTape, main)

import DataGrid.StackedBarChart exposing (render)
import DataGrid.ChartConfig as Cfg
    exposing
        ( defaultStackedBarChartSpec
        , defaultStdChartCfg
        , defaultTooltips
        )
import SampleData.StackedBarChartSample as StackedBarChartSample
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
    case defaultStackedBarChartSpec of
        Cfg.StackedBarChartSpec d ->
            Cfg.StackedBarChartSpec d

        _ ->
            defaultStackedBarChartSpec



--------------------------------------------------------------------------------
-- Data


dataByTape : List (Cfg.StdSeries String)
dataByTape =
    StackedBarChartSample.dataByTape


dataByGroup : List (Cfg.StdSeries String)
dataByGroup =
    StackedBarChartSample.dataByGroup
