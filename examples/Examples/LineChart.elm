module Examples.LineChart exposing (main)

import TypedSvg.Core exposing ( Svg )

import DataGrid.BarChart exposing ( render )
import DataGrid.Config as Cfg exposing ( defaultStdChartCfg, defaultTooltips )
import SampleData.LineChartSample


--------------------------------------------------------------------------------

main : Svg msg
main =
    render cfg data

cfg : Cfg.StdChartCfg String
cfg =
    { defaultStdChartCfg |
      chartSpec = Cfg.defaultBarChartSpec
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
data = LineChartSample.data
