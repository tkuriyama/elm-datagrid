module Examples.ChartGrid exposing ( main )

import Browser
import Element

import DataGrid.Config as Cfg
import DataGrid.ChartGrid as ChartGrid exposing ( chartGrid, defaultLayoutCfg )
import Examples.BarChart as BC
import Examples.LineChart as LC

--------------------------------------------------------------------------------

main =
    ChartGrid.chartGrid cfg charts

cfg : ChartGrid.LayoutCfg
cfg =
    { defaultLayoutCfg |
      title = Just "Demo Chart Grid"
    , description = Just "US equity market statistics"
    , links = []
    }

charts : List (List (ChartGrid.ChartCell String))
charts =
    let f i = { title = Just <| "Chart " ++ String.fromInt i
              , description = Just <| "description " ++ String.fromInt i
              , links = []
              , chartCfg = Cfg.Std BC.cfg
              , chartData = Cfg.BarChartData BC.data
              , showSeries = []
              , showRelative = False
              , showFirstDeriv = False
              }
        lineCharts =
            [ { title = Just "Venue Mkt Share: Large"
              , description = Just <| "Mkt share > 3%"
              , links = []
              , chartCfg = Cfg.Std LC.cfg
              , chartData = Cfg.LineChartData <| LC.filterData 3.0 100.0
              , showSeries = []
              , showRelative = False
              , showFirstDeriv = False
              }
            , { title = Just "Venue Mkt Share: Medium"
              , description = Just "Mkt share 1 - 3%"
              , links = []
              , chartCfg = Cfg.Std LC.cfg
              , chartData = Cfg.LineChartData <| LC.filterData 1.0 3.0
              , showSeries = []
              , showRelative = False
              , showFirstDeriv = False
              }
            , { title = Just "Venue Mkt Share: Small"
              , description = Just "Mkt Share < 1%"
              , links = []
              , chartCfg = Cfg.Std LC.cfg
              , chartData = Cfg.LineChartData <| LC.filterData 0.0 1.0
              , showSeries = []
              , showRelative = False
              , showFirstDeriv = False
              }
            ]
    in [ List.map f [1, 2, 3]
        , lineCharts
       ]
