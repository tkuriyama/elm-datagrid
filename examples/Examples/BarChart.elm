module Examples.BarChart exposing (cfg, data, main)

import DataGrid.BarChart exposing (render)
import DataGrid.Config as Cfg exposing (defaultPadding, defaultStdChartCfg, defaultTooltips)
import SampleData.BarChartSample as BarChartSample
import TypedSvg.Core exposing (Svg)


--------------------------------------------------------------------------------


main : Svg msg
main =
    render cfg data


cfg : Cfg.StdChartCfg String
cfg =
    { defaultStdChartCfg
        | pad = paddingCfg
        , chartSpec = Cfg.defaultBarChartSpec
        , showLabels = False
        , labelFormatter = identity
        , tooltips = tooltipsCfg
    }


paddingCfg : Cfg.Padding
paddingCfg =
    { defaultPadding
        | right = 50
    }


tooltipsCfg : Cfg.Tooltips
tooltipsCfg =
    { defaultTooltips
        | showLargeTooltips = True
    }


data : Cfg.StdSeriesPair String
data =
    BarChartSample.data
        |> List.head
        |> Maybe.withDefault ( "Empty", [] )
