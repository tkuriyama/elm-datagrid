module Examples.ChartGrid exposing (main)

import Element
import Html exposing (Html)

import DataGrid.Layout as Layout exposing ( efaultLayoutCfg )
import Examples.BarChart as BC
import Examples.LineChart as LC

--------------------------------------------------------------------------------

main : Html msg
main =
    Layout.chartGrid cfg charts

cfg : Layout.LayoutConfig
cfg =
    { defaultLayoutConfig |
      title = Just "Demo Chart Grid"
    , description = Just "US equity market statistics"
    , links = []
    }

charts : List (List (Layout.Chart msg))
charts =
    let f i = { title = Just <| "Chart " ++ String.fromInt i
              , description = Just <| "description " ++ String.fromInt i
              , links = []
              , chartCfg = BC.cfg
              , chartData = BC.data
              }
        lineCharts =
            [ { title = Just "Venue Mkt Share: Large"
              , description = Just <| "Mkt share > 3%"
              , links = []
              , chartCfg = LC.cfg
              , chartData = LC.filterData 3.0 100.0
              }
            , { title = Just "Venue Mkt Share: Medium"
              , description = Just "Mkt share 1 - 3%"
              , links = []
              , chartCfg = LC.cfg
              , chartData = LC.filterData 1.0 3.0
              }
            , { title = Just "Venue Mkt Share: Small"
              , description = Just "Mkt Share < 1%"
              , links = []
              , chartCfg = LC.cfg
              , chartData = LC.filterData 0.0 1.0
              }
            ]
    in [ List.map f [1, 2, 3]
        , lineCharts
       ]
