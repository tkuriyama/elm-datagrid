module Examples.LineChart exposing (cfg, filterData, main)

import DataGrid.Config as Cfg
    exposing
        ( defaultLineChartSpec
        , defaultPadding
        , defaultStdChartCfg
        , defaultTooltips
        )
import DataGrid.Internal.Utils as Utils
import DataGrid.LineChart exposing (render)
import List.Extra as LE
import SampleData.LineChartSample as LineChartSample
import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


main : Svg msg
main =
    chart 0.0 1.0


chart : Float -> Float -> Svg msg
chart lower upper =
    let
        d =
            filterData lower upper
    in
    render cfg d


cfg : Cfg.StdChartCfg String
cfg =
    { defaultStdChartCfg
        | pad = paddingCfg
        , chartSpec = lineChartSpec
        , dataAxisTicks = 10
        , showLabels = False
        , labelFormatter = identity
        , tooltips = tooltipsCfg
    }


paddingCfg : Cfg.Padding
paddingCfg =
    { defaultPadding
        | right = 80
    }


tooltipsCfg : Cfg.Tooltips
tooltipsCfg =
    { defaultTooltips
        | showTooltips = False
        , showLargeTooltips = False
        , showHoverTooltips = True
        , hoverTooltipSize = 16
    }


lineChartSpec : Cfg.ChartSpec
lineChartSpec =
    case defaultLineChartSpec of
        Cfg.LineChartSpec d ->
            Cfg.LineChartSpec
                { d
                    | lineNameSize = 14
                }

        _ ->
            defaultLineChartSpec



--------------------------------------------------------------------------------
-- Data


filterData : Float -> Float -> Cfg.StdSeriesPairs String
filterData lower upper =
    let
        last =
            Utils.snd >> Utils.snds >> LE.last >> Maybe.withDefault 0

        f p =
            let
                last_ =
                    last p
            in
            last_ > lower && last_ <= upper
    in
    List.filter f data
        |> Utils.fsts
        |> List.filter (String.contains "TRF" >> not)
        |> subset


subset : List String -> Cfg.StdSeriesPairs String
subset venues =
    data |> List.filter (\p -> List.member (Utils.fst p) venues)


data : Cfg.StdSeriesPairs String
data =
    let
        x100 n =
            n * 100

        f ( name, pairs ) =
            ( name, List.map (Tuple.mapSecond x100) pairs )
    in
    List.map f LineChartSample.data
