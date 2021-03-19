module TestLineChart exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import DataGrid.LineChart exposing (..)

--------------------------------------------------------------------------------
-- Reshaping

testTranspose : Test
testTranspose =
    let sp1 = [ ("Name1", [("Label1", 1.0), ("Label2", 2.0)])
              ]
        sp2 = [ ("Label1", [ ("Name1", 1.0) ])
              , ("Label2", [ ("Name1", 2.0) ])
              ]
        sp3 = [ ("Name1", [ ("Label1", 1.0)
                          , ("Label2", 2.0)
                          ]
                )
              , ("Name2", [ ("Label1", 11.0)
                          , ("Label2", 12.0)
                          ]
                )
              ]
        sp4 = [ ("Label1", [ ("Name1", 1.0)
                           ,  ("Name2", 11.0)
                           ]
                )
              , ("Label2", [ ("Name1", 2.0)
                           , ("Name2", 12.0)
                           ]
                )
              ]
    in describe "Test reshape SeriesPairs with one name"
        [ test "reshape sample" <|
              \_ -> transpose sp1 |> Expect.equal sp2
        ,
            test "reshape sample with more than one name" <|
                \_ -> transpose sp3 |> Expect.equal sp4
        ]

testToMatrix : Test
testToMatrix =
    let sp3 = [ ("Name1", [ ("Label1", 1.0)
                          , ("Label2", 2.0)
                          ]
                )
              , ("Name2", [ ("Label1", 11.0)
                          , ("Label2", 12.0)
                          ]
                )
              ]
    in describe "Test toMatrix"
        [ test "toMatrix sample" <|
              \_ -> toMatrix sp3 |>
              Expect.equal [[1.0, 2.0], [11.0, 12.0]]
        ]


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
