module Examples.ChartGrid exposing (main)

import DataGrid.ChartGrid as ChartGrid
    exposing
        ( defaultChartCell
        , defaultLayoutCfg
        )
import DataGrid.ChartGrid.Types exposing (ChartCell, ChartGrid(..), LayoutCfg)
import DataGrid.Config as Cfg
import Examples.BarChart as BC
import Examples.BarChartStacked as BCS
import Examples.LineChart as LC



--------------------------------------------------------------------------------


main =
    ChartGrid.chartGrid cfg charts


cfg : LayoutCfg
cfg =
    { defaultLayoutCfg
        | w = Just 1900
        , title = Just "Demo Chart Grid"
        , description = Just "US equity market statistics"
        , links =
            [ ( "CBOE"
              , "https://certification.batstrading.com/market_summary/"
              )
            , ( "Source"
              , "https://github.com/tkuriyama/elm-datagrid"
              )
            ]
    }


charts : ChartGrid String
charts =
    Column ( Nothing, Nothing )
        [ Row ( Nothing, Nothing )
            [ Cell totalMkt
            , Cell totalMktNotional
            ]
        , Row ( Nothing, Nothing )
            [ TabbedCell "Shares"
                [ ( "Shares", totalMkt )
                , ( "Shares by Tape", totalMktByTape )
                , ( "Shares by Group", totalMktByGroup )
                , ( "Notional", totalMktNotional )
                ]
            , TabbedCell "Large"
                [ ( "TRF", lineChartTRF )
                , ( "Large", lineChartLarge )
                , ( "Medum", lineChartMed )
                , ( "Small", lineChartSmall )
                ]
            ]
        ]



--------------------------------------------------------------------------------
-- Cells


totalMkt : ChartCell String
totalMkt =
    { defaultChartCell
        | title = Just <| "Total Market Volume"
        , description = Just <| "in billions of shares"
        , chartCfg = Cfg.Std BC.cfg
        , chartData = Cfg.BarChartData BC.dataShares
    }


totalMktNotional : ChartCell String
totalMktNotional =
    { defaultChartCell
        | title = Just <| "Total Market Value"
        , description = Just <| "in billions of USD"
        , chartCfg = Cfg.Std BC.cfg
        , chartData = Cfg.BarChartData BC.dataNotional
    }


totalMktByTape : ChartCell String
totalMktByTape =
    { defaultChartCell
        | title = Just <| "Market Volume by Tape"
        , description = Just <| "in billions of shares"
        , chartCfg = Cfg.Std BCS.cfg
        , chartData = Cfg.BarChartStackedData BCS.dataByTape
    }


totalMktByGroup : ChartCell String
totalMktByGroup =
    { defaultChartCell
        | title = Just <| "Market Volume by Group"
        , description = Just <| "in billions of shares"
        , chartCfg = Cfg.Std BCS.cfg
        , chartData = Cfg.BarChartStackedData BCS.dataByGroup
    }


lineChart : ( String, String, Cfg.StdSeriesPairs String ) -> ChartCell String
lineChart ( title, desc, data ) =
    { defaultChartCell
        | title = Just title
        , description = Just desc
        , chartCfg = Cfg.Std LC.cfg
        , chartData = Cfg.LineChartData data
    }


lineChartTRF : ChartCell String
lineChartTRF =
    let
        cfg_ =
            LC.cfg

        padding =
            LC.cfg.pad
    in
    { defaultChartCell
        | title = Just "Venue Mkt Share: TRF"
        , description = Nothing
        , chartCfg = Cfg.Std { cfg_ | pad = { padding | right = 170 } }
        , chartData = Cfg.LineChartData <| LC.filterData 0.0 100.0 True
    }


lineChartLarge : ChartCell String
lineChartLarge =
    lineChart
        ( "Venue Mkt Share: Large"
        , "mkt share > 3%"
        , LC.filterData 3.0 100.0 False
        )


lineChartMed : ChartCell String
lineChartMed =
    lineChart
        ( "Venue Mkt Share: Medium"
        , "mkt share 1 - 3%"
        , LC.filterData 1.0 3.0 False
        )


lineChartSmall : ChartCell String
lineChartSmall =
    lineChart
        ( "Venue Mkt Share: Small"
        , "mkt share <= 1%"
        , LC.filterData 0.0 1.0 False
        )
