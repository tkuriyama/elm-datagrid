module TestLineChart exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import DataGrid.LineChart exposing (..)


--------------------------------------------------------------------------------

testProjectFirstDeriv : Test
testProjectFirstDeriv =
    describe "Test projectFirstDeriv"
        [ test "delta 1" <|
              \_ -> projectFirstDeriv [ ( "series"
                                        , [ ('a', 1.0)
                                          , ('b', 2.0)
                                          , ('c', 3.0)
                                          ]
                                        )
                                      ] |>
              Expect.equal [ ( "series"
                             , [ ('a', 0.0)
                               , ('b', 1.0)
                               , ('c', 1.0)
                               ]
                             )
                           ]
        ]

testOffsetDelta : Test
testOffsetDelta =
    describe "Test offset delta"
        [ test "delta 1" <|
              \_ -> offsetDelta 1 [ ('a', 1.0)
                                  , ('b', 2.0)
                                  , ('c', 3.0)
                                  ] |>
              Expect.equal [ ('a', 0.0)
                                , ('b', 1.0)
                                , ('c', 1.0)
                                ]
        ]
