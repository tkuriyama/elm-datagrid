module Examples.ChartGrid exposing (main)

import Element
import Html exposing (Html)

import DataGrid.Layout as Layout
import Examples.BarChart as BC
import Examples.LineChart as LC

--------------------------------------------------------------------------------

main : Html msg
main =
    Layout.chartGrid cfg charts

cfg : Layout.LayoutConfig
cfg =
    { w = 1800
    , colSpacing = 0
    , rowSpacing = 5
    , padding = 5
    , title = Just "Demo Chart Grid"
    , description = Just "US equity market statistics"
    , links = []
    , textColor = Nothing
    , typeface = Just "Consolas"
    , gridBaseFontSize = 22
    , cellBaseFontSize = 16
    }

charts : List (List (Layout.Chart msg))
charts =
    let f i = { title = Just <| "Chart " ++ String.fromInt i
              , description = Just <| "description " ++ String.fromInt i
              , links = []
              , chartSpec = BC.cfg.chartSpec
              , chart = BC.main |> Element.html
              }
        lineCharts =
            [ { title = Just "Venue Mkt Share: Large"
              , description = Just <| "Mkt share > 3%"
              , links = []
              , chartSpec = LC.cfg.chartSpec
              , chart = LC.chart 3.0 100.0 |> Element.html
              }
            , { title = Just "Venue Mkt Share: Medium"
              , description = Just "Mkt share 1 - 3%"
              , links = []
              , chartSpec = LC.cfg.chartSpec
              , chart = LC.chart 1.0 3.0 |> Element.html
              }
            , { title = Just "Venue Mkt Share: Small"
              , description = Just "Mkt Share < 1%"
              , links = []
              , chartSpec = LC.cfg.chartSpec
              , chart = LC.chart 0.0 1.0 |> Element.html
              }
            ]
    in [ List.map f [1, 2, 3]
        , lineCharts
       ]
