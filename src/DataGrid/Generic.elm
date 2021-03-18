module DataGrid.Generic exposing ( render )

{-| Generic operations for working with top-level chart types.

-}

import TypedSvg exposing ( svg )
import TypedSvg.Core exposing ( Svg )

import DataGrid.BarChart as BC
import DataGrid.Config as Cfg exposing ( ChartCfg(..), ChartData(..) )
import DataGrid.LineChart as LC


--------------------------------------------------------------------------------

render : Cfg.ChartCfg label -> ChartData label -> Svg msg
render cfg data =
    case cfg of
        Std stdCfg -> renderStd stdCfg data
        DefaultChartCfg -> svg [] []

renderStd : Cfg.StdChartCfg label -> ChartData label -> Svg msg
renderStd cfg data =
    case data of
        BarChartData d -> BC.render cfg d
        LineChartData d -> LC.render cfg d
        DefaultData -> svg [] []
