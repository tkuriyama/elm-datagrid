module Examples.GridChart exposing (cfg, data, main)

import DataGrid.GridChart exposing (render)
import DataGrid.Config as Cfg exposing (defaultPadding, defaultGridChartCfg, defaultTooltips)
import SampleData.GridChartSample as GridChartSample
import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


main : Svg msg
main =
    render cfg data


cfg : Cfg.GridChartCfg
cfg =
    { defaultGridChartCfg
        | pad = paddingCfg
        , chartSpec = Cfg.defaultGridChartSpec
    }


paddingCfg : Cfg.Padding
paddingCfg =
    { defaultPadding
        | right = 50
    }


tooltipsCfg : Cfg.Tooltips
tooltipsCfg =
    { defaultTooltips
        | showTooltips = False
        , showHoverTooltips = True
    }

data : List Cfg.GridSeries
data =
    GridChartSample.data
