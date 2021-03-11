module Examples.BarChartGrid exposing (main)

import Element
import Html exposing (Html)

import DataGrid.Layout as Layout
import Examples.BarChart as BC


--------------------------------------------------------------------------------

main : Html msg
main =
    Layout.chartGrid cfg charts

cfg : Layout.LayoutConfig
cfg =
    { w = 1800
    , colSpacing = 0
    , rowSpacing = 5
    , padding = 0
    , title = Just "Demo Chart Grid"
    , description = Just "Demo chart grid description here."
    , fontColor = Nothing
    , fontFamily = Nothing
    , gridBaseFontSize = 24
    , cellBaseFontSize = 18
    }

charts : List (List (Layout.Chart msg))
charts =
    let labels = [ [ 1, 2, 3 ]
                 , [ 4, 5 ]
--                 , [ 7, 8 ]
                 ]
        f i = { title = Just <| "Chart " ++ (String.fromInt i)
              , description = Just <| "descrption " ++ (String.fromInt i)
              , chart = BC.main |> Element.html
              }
    in List.map (List.map f) labels
