module Examples.LineChart exposing (main)

import TypedSvg.Core exposing ( Svg )

import DataGrid.LineChart exposing ( render )
import DataGrid.Config as Cfg exposing ( defaultStdChartCfg, defaultTooltips )
import SampleData.LineChartSample as LineChartSample


--------------------------------------------------------------------------------

main : Svg msg
main =
    render cfg data

cfg : Cfg.StdChartCfg String
cfg =
    { defaultStdChartCfg |
      chartSpec = Cfg.defaultLineChartSpec
    , dataAxisTicks = 10
    , showLabels = False
    , labelFormatter = identity
    , tooltips = tooltipsCfg
    }

tooltipsCfg : Cfg.Tooltips
tooltipsCfg =
    { defaultTooltips |
     showLargeTooltips = True
    }

--------------------------------------------------------------------------------

data : List (String, List (String, Float))
data =
    let x100 n = n * 100
        f (name, pairs) = (name, List.map (Tuple.mapSecond x100) pairs)
    in LineChartSample.data |> List.map f
