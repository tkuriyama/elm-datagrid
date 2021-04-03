module Examples.GridChart exposing (cfg, dataByGroup, dataByVenue, main)

import DataGrid.ChartConfig as Cfg exposing (defaultGridChartCfg, defaultPadding, defaultTooltips)
import DataGrid.GridChart exposing (render)
import SampleData.GridChartSample as GridChartSample
import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


main : Svg msg
main =
    render cfg dataByVenue


cfg : Cfg.GridChartCfg
cfg =
    { defaultGridChartCfg
        | pad = paddingCfg
        , chartSpec = Cfg.defaultGridChartSpec
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


dataByVenue : List Cfg.GridSeries
dataByVenue =
    GridChartSample.dataByVenue


dataByGroup : List Cfg.GridSeries
dataByGroup =
    GridChartSample.dataByGroup
