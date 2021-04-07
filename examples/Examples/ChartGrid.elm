module Examples.ChartGrid exposing (main)

import DataGrid.ChartConfig as Cfg
import DataGrid.ChartGrid as ChartGrid
    exposing
        ( defaultChartCell
        , defaultLayoutCfg
        )
import DataGrid.GridConfig exposing (ChartCell, ChartGrid(..), LayoutCfg)
import Examples.BarChart as BC
import Examples.StackedBarChart as SBC
import Examples.FacetGridChart as FGC
import Examples.LineChart as LC



--------------------------------------------------------------------------------


main =
    ChartGrid.chartGrid cfg charts


cfg : LayoutCfg
cfg =
    { defaultLayoutCfg
        | w = Just 1950
        , title = Just "US Equities"
        , description = Just "market summary"
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
            [ TabbedCell "By Venue"
                [ ( "By Group", groupGrid )
                , ( "By Venue", venueGrid )
                ]
            , TabbedCell "Placeholder"
                [ ( "Placeholder", totalMktNotional )
                ]
            ]
        , Row ( Nothing, Nothing )
            [ TabbedCell "Shares"
                [ ( "Shares", totalMkt )
                , ( "Shares by Tape", totalMktByTape )
                , ( "Shares by Group", totalMktByGroup )
                , ( "Notional", totalMktNotional )
                ]
            , TabbedCell "Large"
                [ ( "All ex TRF", lineChartAllExTRF )
                , ( "TRF", lineChartTRF )
                , ( "Large", lineChartLarge )
                , ( "Medum", lineChartMed )
                , ( "Small", lineChartSmall )
                ]
            ]
        ]



--------------------------------------------------------------------------------
-- Cells


groupGrid : ChartCell String
groupGrid =
    let
        cfg_ =
            FGC.cfg

        spec =
            Cfg.defaultFacetGridChartSpec

        spec_ =
            case spec of
                Cfg.FacetGridChartSpec s ->
                    Cfg.FacetGridChartSpec { s | showHBar = False }

                _ ->
                    Cfg.DefaultSpec
    in
    { defaultChartCell
        | title = Just <| "Yesterday: Mkt Share by Group"
        , description = Just <| "trailing 60, 20, 1 day means"
        , chartCfg = Cfg.Grid { cfg_ | chartSpec = spec_ }
        , chartData = Cfg.FacetGridChartData FGC.dataByGroup
    }


venueGrid : ChartCell String
venueGrid =
    { defaultChartCell
        | title = Just <| "Yesterday: Mkt Share by Venue"
        , description = Just <| "trailing 60, 20, 1 day means"
        , chartCfg = Cfg.Grid FGC.cfg
        , chartData = Cfg.FacetGridChartData FGC.dataByVenue
    }


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
        , chartCfg = Cfg.Std SBC.cfg
        , chartData = Cfg.StackedBarChartData SBC.dataByTape
    }


totalMktByGroup : ChartCell String
totalMktByGroup =
    { defaultChartCell
        | title = Just <| "Market Volume by Group"
        , description = Just <| "in billions of shares"
        , chartCfg = Cfg.Std SBC.cfg
        , chartData = Cfg.StackedBarChartData SBC.dataByGroup
    }


lineChart : ( String, String, List (Cfg.StdSeries String) ) -> ChartCell String
lineChart ( title, desc, data ) =
    { defaultChartCell
        | title = Just title
        , description = Just desc
        , chartCfg = Cfg.Std LC.cfg
        , chartData = Cfg.LineChartData data
    }


lineChartAllExTRF : ChartCell String
lineChartAllExTRF =
    lineChart
        ( "Venue Mkt Share: All ex. TRF"
        , ""
        , LC.filterData 0.0 100.0 False
        )


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
        , LC.filterData -0.1 1.0 False
        )
