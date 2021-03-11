-- Pointer to specific example modules...

module Main exposing (main)

import Html exposing (Html)

-- import Examples.BarChart as BarChart
import Examples.BarChartGrid as BarChartGrid


--------------------------------------------------------------------------------

main : Html msg
main = BarChartGrid.main
