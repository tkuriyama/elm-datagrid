module Examples.LineChart exposing ( main, chart )

import List.Extra as LE
import TypedSvg.Core exposing ( Svg )

import DataGrid.LineChart exposing ( render )
import DataGrid.Config as Cfg exposing ( defaultStdChartCfg
                                       , defaultPadding, defaultTooltips )
import Internal.Utils as Utils
import SampleData.LineChartSample as LineChartSample

--------------------------------------------------------------------------------

main : Svg msg
main =
    chart 0.0 1.0

chart : Float -> Float -> Svg msg
chart lower upper =
    let d = filterData lower upper
    in render cfg d

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
    , hoverTooltipSize = 14
    }

lineChartSpec : Cfg.ChartSpec
lineChartSpec =
    Cfg.LineChartSpec
        { showLineName = True
        , lineNameSize = 14
        , showVBar = True
        }


--------------------------------------------------------------------------------
-- Data

filterData : Float -> Float -> List (String, List (String, Float))
filterData lower upper =
    let last = Utils.snd >> Utils.snds >> LE.last >> Maybe.withDefault 0
        f p = let last_ = last p
              in last_ > lower && last_ <= upper
    in List.filter f data
        |> Utils.fsts
        |> List.filter (String.contains "TRF" >> not)
        |> subset

subset : List String -> List (String, List (String, Float))
subset venues =
    data |> List.filter (\p -> List.member (Utils.fst p) venues)

data : List (String, List (String, Float))
data =
    let x100 n = n * 100
        f (name, pairs) = (name, List.map (Tuple.mapSecond x100) pairs)
    in List.map f LineChartSample.data
