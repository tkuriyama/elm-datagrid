module TestChartGrid exposing (..)

import DataGrid.ChartGrid exposing (..)
import DataGrid.ChartGrid.Types exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)



--------------------------------------------------------------------------------


testReindex : Test
testReindex =
    let
        cell =
            defaultChartCell
    in
    describe "Test reindex"
        [ test "reindex Cell" <|
            \_ ->
                reindex 1 (Cell cell)
                    |> Expect.equal
                        (Cell { cell | index = 1 })
        , test "reindex TabbedCell" <|
            \_ ->
                reindex 1
                    (TabbedCell ""
                        [ ( "", cell )
                        , ( "", cell )
                        ]
                    )
                    |> Expect.equal
                        (TabbedCell ""
                            [ ( "", { cell | index = 100 } )
                            , ( "", { cell | index = 101 } )
                            ]
                        )
        , test "reindex Row -> Cell" <|
            \_ ->
                reindex 1
                    (Row ( Nothing, Nothing )
                        [ Column ( Nothing, Nothing ) [ Cell cell ]
                        , Column ( Nothing, Nothing ) [ Cell cell ]
                        ]
                    )
                    |> Expect.equal
                        (Row ( Nothing, Nothing )
                            [ Column ( Nothing, Nothing )
                                [ Cell { cell | index = 10000 } ]
                            , Column ( Nothing, Nothing )
                                [ Cell { cell | index = 10100 } ]
                            ]
                        )
        ]
