module DataGrid.Internal.Generic exposing (render)

import DataGrid.BarChart as BC
import DataGrid.ChartConfig as Cfg exposing (ChartCfg(..), ChartData(..))
import DataGrid.FacetGridChart as FGC
import DataGrid.TreeGridChart as TGC
import DataGrid.LineChart as LC
import DataGrid.StackedBarChart as SBC
import TypedSvg exposing (svg)
import TypedSvg.Core exposing (Svg)



--------------------------------------------------------------------------------


render : Cfg.ChartCfg label -> ChartData label -> Svg msg
render cfg data =
    case cfg of
        Std stdCfg ->
            renderStd stdCfg data

        Grid gridCfg ->
            renderGrid gridCfg data

        DefaultChartCfg ->
            svg [] []


renderStd : Cfg.StdChartCfg label -> ChartData label -> Svg msg
renderStd cfg data =
    case data of
        BarChartData d ->
            BC.render cfg d

        StackedBarChartData d ->
            SBC.render cfg d

        LineChartData d ->
            LC.render cfg d

        _ ->
            svg [] []


renderGrid : Cfg.GridChartCfg -> ChartData label -> Svg msg
renderGrid cfg data =
    case data of
        FacetGridChartData d ->
            FGC.render cfg d

        TreeGridChartData d ->
            TGC.render cfg d

        _ ->
            svg [] []
