module Examples.LineChart exposing (main)

import TypedSvg.Core exposing ( Svg )

import DataGrid.LineChart exposing ( render )
import DataGrid.Config as Cfg exposing ( defaultStdChartCfg
                                       , defaultPadding, defaultTooltips )
import SampleData.LineChartSample as LineChartSample


--------------------------------------------------------------------------------

main : Svg msg
main =
    render cfg data

cfg : Cfg.StdChartCfg String
cfg =
    { defaultStdChartCfg |
      pad = paddingCfg
    , chartSpec = lineChartSpec
    , dataAxisTicks = 10
    , showLabels = False
    , labelFormatter = identity
    , tooltips = tooltipsCfg
    }

paddingCfg : Cfg.Padding
paddingCfg =
    { defaultPadding |
      right = 80
    }

tooltipsCfg : Cfg.Tooltips
tooltipsCfg =
    { defaultTooltips |
      showLargeTooltips = False
    , showHoverTooltips = True
    }

lineChartSpec : Cfg.ChartSpec
lineChartSpec =
    Cfg.LineChartSpec
        { showLineName = True
        , lineNameSize = 14
        , showVBar = True
        }


--------------------------------------------------------------------------------

data : List (String, List (String, Float))
data =
    let x100 n = n * 100
        f (name, pairs) = (name, List.map (Tuple.mapSecond x100) pairs)
    in LineChartSample.data |> List.map f
