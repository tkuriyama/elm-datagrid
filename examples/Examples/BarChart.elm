module Examples.BarChart exposing (cfg, data, main)

import DataGrid.BarChart exposing (render)
import DataGrid.Config as Cfg exposing (defaultStdChartCfg, defaultTooltips)
import SampleData.BarChartSample as BarChartSample
import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


main : Svg msg
main =
    render cfg data


cfg : Cfg.StdChartCfg String
cfg =
    { defaultStdChartCfg
        | chartSpec = Cfg.defaultBarChartSpec
        , showLabels = False
        , labelFormatter = identity
        , tooltips = tooltipsCfg
    }


tooltipsCfg : Cfg.Tooltips
tooltipsCfg =
    { defaultTooltips
        | showLargeTooltips = True
    }


data : Cfg.StdSeriesPair String
data =
    BarChartSample.data
        |> List.map (Tuple.mapSecond (List.map (Tuple.mapSecond billions)))
        |> List.head
        |> Maybe.withDefault ( "Empty", [] )


billions : Float -> Float
billions x =
    x / 1000000000.0
