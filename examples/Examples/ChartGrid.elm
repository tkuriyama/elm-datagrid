module Examples.ChartGrid exposing (main)

import DataGrid.ChartGrid as ChartGrid
    exposing
        ( chartGrid
        , defaultChartCell
        , defaultLayoutCfg
        )
import DataGrid.Config as Cfg
import Examples.BarChart as BC
import Examples.LineChart as LC



--------------------------------------------------------------------------------


main =
    ChartGrid.chartGrid cfg charts


cfg : ChartGrid.LayoutCfg
cfg =
    { defaultLayoutCfg
        | title = Just "Demo Chart Grid"
        , description = Just "US equity market statistics"
        , links =
            [ ( "CBOE Bats"
              , "https://certification.batstrading.com/market_summary/"
              )
            , ( "Source"
              , "https://github.com/tkuriyama/elm-datagrid"
              )
            ]
    }


charts : List (List (ChartGrid.ChartCell String))
charts =
    let
        f i =
            { defaultChartCell
                | title = Just <| "Chart " ++ String.fromInt i
                , description = Just <| "description " ++ String.fromInt i
                , chartCfg = Cfg.Std BC.cfg
                , chartData = Cfg.BarChartData BC.data
            }

        g ( title, desc, data ) =
            { defaultChartCell
                | title = Just title
                , description = Just desc
                , chartCfg = Cfg.Std LC.cfg
                , chartData = Cfg.LineChartData data
            }
    in
    [ List.map f [ 1, 2, 3 ]
    , List.map g
        [ ( "Venue Mkt Share: Large"
          , "Mkt share > 3%"
          , LC.filterData 3.0 100.0
          )
        , ( "Venue Mkt Share: Medium"
          , "Mkt share 1 - 3%"
          , LC.filterData 1.0 3.0
          )
        , ( "Venue Mkt Share: Small"
          , "Mkt share < 1%"
          , LC.filterData 0.0 1.0
          )
        ]
    ]
