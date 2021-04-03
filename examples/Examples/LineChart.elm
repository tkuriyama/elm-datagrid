module Examples.LineChart exposing (cfg, filterData, main)

import DataGrid.ChartConfig as Cfg
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
    chart 3.0 100.0 True


chart : Float -> Float -> Bool -> Svg msg
chart lower upper ignoreTRF =
    let
        d =
            filterData lower upper ignoreTRF
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


filterData : Float -> Float -> Bool -> Cfg.StdSeriesPairs String
filterData lower upper containsTRF =
    let
        last =
            Utils.snd >> Utils.snds >> LE.last >> Maybe.withDefault 0

        f p =
            let
                last_ =
                    last p
            in
            last_ > lower && last_ <= upper

        filterTRF =
            String.contains "TRF"
                >> (if containsTRF then
                        identity

                    else
                        not
                   )
    in
    List.filter f data
        |> Utils.fsts
        |> List.filter filterTRF
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
