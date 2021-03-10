module Examples.BarChartGrid exposing (main)

import Element
import Html exposing (Html)

import DataGrid.Layout as Layout
import Examples.BarChart as BC


--------------------------------------------------------------------------------

main : Html msg
main =
    Layout.chartGrid 0 0 charts

cfg : Layout.LayoutConfig
cfg = 
    { w = 1800
    , spacing = 10
    , title = Just "Demo Chart Grid"
    , description = Just "Demo chart grid description here."
    , gridBaseFontSize = 20
    , cellBaseFontSize = 14
    }

charts : List (Layout.Chart msg)
charts = [ { title = Just "Chart 1"
           , description = Just "descrption 1"
           , chart = BC.main |> Element.html
           }
         , { title = Just "Chart 2"
           , description = Just "descrption 2"
           , chart = BC.main |> Element.html
           }
         , { title = Just "Chart 3"
           , description = Just "descrption 3"
           , chart = BC.main |> Element.html
           }
         ]
