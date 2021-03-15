-- Pointer to specific example modules...

module Main exposing (main)

import Html exposing (Html)

-- import Examples.BarChart as BarChart
--import Examples.LineChart as LineChart
import Examples.ChartGrid as ChartGrid


--------------------------------------------------------------------------------

main : Html msg
main = ChartGrid.main
