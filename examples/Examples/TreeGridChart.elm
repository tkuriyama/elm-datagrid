module Examples.TreeGridChart exposing (cfg, dataIWC, dataIVV, dataIJR, main)

import DataGrid.ChartConfig as Cfg exposing (defaultGridChartCfg, defaultPadding, defaultTooltips)
import DataGrid.TreeGridChart exposing (render)
import SampleData.TreeGridChartSample as TreeGridChartSample
import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


main : Svg msg
main =
    render cfg dataIWC


cfg : Cfg.GridChartCfg
cfg =
    { defaultGridChartCfg
        | pad = paddingCfg
        , chartSpec = Cfg.defaultTreeGridChartSpec
    }


paddingCfg : Cfg.Padding
paddingCfg =
    { defaultPadding
        | top = 10
    }


tooltipsCfg : Cfg.Tooltips
tooltipsCfg =
    { defaultTooltips
        | showTooltips = False
        , showHoverTooltips = True
    }


dataIVV : List (Cfg.GridSeries Cfg.GridTriple)
dataIVV =
    TreeGridChartSample.dataIVV


dataIJR : List (Cfg.GridSeries Cfg.GridTriple)
dataIJR =
    TreeGridChartSample.dataIJR

dataIWC : List (Cfg.GridSeries Cfg.GridTriple)
dataIWC =
    TreeGridChartSample.dataIWC
