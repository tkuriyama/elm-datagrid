module Examples.FacetGridChart exposing (cfg, dataByGroup, dataByVenue, main)

import DataGrid.ChartConfig as Cfg exposing (defaultGridChartCfg, defaultPadding, defaultTooltips)
import DataGrid.FacetGridChart exposing (render)
import SampleData.FacetGridChartSample as FacetGridChartSample
import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


main : Svg msg
main =
    render cfg dataByVenue


cfg : Cfg.GridChartCfg
cfg =
    { defaultGridChartCfg
        | pad = paddingCfg
        , chartSpec = Cfg.defaultFacetGridChartSpec
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
    FacetGridChartSample.dataByVenue


dataByGroup : List Cfg.GridSeries
dataByGroup =
    FacetGridChartSample.dataByGroup
