module Examples.BarChart exposing (cfg, dataNotional, dataShares, main)

import DataGrid.BarChart exposing (render)
import DataGrid.ChartConfig as Cfg exposing (defaultPadding, defaultStdChartCfg, defaultTooltips)
import SampleData.BarChartSample as BarChartSample
import TypedSvg.Core exposing (Svg)


--------------------------------------------------------------------------------


main : Svg msg
main =
    render cfg dataShares


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


dataShares : Cfg.StdSeries String
dataShares =
    BarChartSample.dataShares
        |> List.head
        |> Maybe.withDefault ( "Empty", [] )


dataNotional : Cfg.StdSeries String
dataNotional =
    BarChartSample.dataNotional
        |> List.head
        |> Maybe.withDefault ( "Empty", [] )
