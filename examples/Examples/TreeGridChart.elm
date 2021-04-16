module Examples.TreeGridChart exposing (cfg, dataIWC, dataIVV, dataIJR, main)

import DataGrid.ChartConfig as Cfg exposing (defaultGridChartCfg, defaultPadding, defaultTooltips)
import DataGrid.TreeGridChart exposing (render)
import SampleData.TreeGridChartSample as TreeGridChartSample
import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


main : Svg msg
main =
    render cfg dataIVV


cfg : Cfg.GridChartCfg
cfg =
    { defaultGridChartCfg
        | pad = paddingCfg
        , chartSpec = Cfg.defaultTreeGridChartSpec
    }


paddingCfg : Cfg.Padding
paddingCfg =
    { defaultPadding
        | top = 20
        , left = 10
        , right = 20
    }


tooltipsCfg : Cfg.Tooltips
tooltipsCfg =
    { defaultTooltips
        | showTooltips = False
        , showHoverTooltips = True
    }


dataIVV : List (Cfg.GridSeries Cfg.GridTriple)
dataIVV =
    TreeGridChartSample.dataIVV |> List.map (Tuple.mapFirst rename)


dataIJR : List (Cfg.GridSeries Cfg.GridTriple)
dataIJR =
    TreeGridChartSample.dataIJR |> List.map (Tuple.mapFirst rename)

dataIWC : List (Cfg.GridSeries Cfg.GridTriple)
dataIWC =
    TreeGridChartSample.dataIWC |> List.map (Tuple.mapFirst rename)



--------------------------------------------------------------------------------

rename : String -> String
rename s =
    case s of
        "Communication" ->
            "Comm."

        "Consumer Discretionary" ->
            "Con. Disc."

        "Consumer Staples" ->
            "Con. Stap."

        "Information Technology" ->
            "I.T."

        "Real Estate" ->
            "R.E."

        "Financials" ->
            "Fin."

        "Industrials" ->
            "Ind."

        "Materials" ->
            "Mat."

        "Health Care" ->
            "Health"

        "Utilities" ->
            "Util."

        _ ->
            s
