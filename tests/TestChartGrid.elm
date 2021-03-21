module TestChartGrid exposing (..)

import DataGrid.ChartGrid exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)



--------------------------------------------------------------------------------


testReindex : Test
testReindex =
    describe "Test reindex"
        [ test "reindex all default (0) indices" <|
            \_ ->
                reindex
                    [ [ { index = 0 }, { index = 0 } ]
                    , [ { index = 0 }, { index = 0 } ]
                    ]
                    |> Expect.equal
                        [ [ { index = 1 }, { index = 2 } ]
                        , [ { index = 1001 }, { index = 1002 } ]
                        ]
        ]
