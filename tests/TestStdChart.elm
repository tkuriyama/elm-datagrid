module TestStdChart exposing (..)

import DataGrid.Internal.Utils as Utils
import DataGrid.Internal.StdChart exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)



--------------------------------------------------------------------------------
-- Reshaping


testTranspose : Test
testTranspose =
    let
        sp1 =
            [ ( "Name1", [ ( "Label1", 1.0 ), ( "Label2", 2.0 ) ] )
            ]

        sp2 =
            [ ( "Label1", [ ( "Name1", 1.0 ) ] )
            , ( "Label2", [ ( "Name1", 2.0 ) ] )
            ]

        sp3 =
            [ ( "Name1"
              , [ ( "Label1", 1.0 )
                , ( "Label2", 2.0 )
                ]
              )
            , ( "Name2"
              , [ ( "Label1", 11.0 )
                , ( "Label2", 12.0 )
                ]
              )
            ]

        sp4 =
            [ ( "Label1"
              , [ ( "Name1", 1.0 )
                , ( "Name2", 11.0 )
                ]
              )
            , ( "Label2"
              , [ ( "Name1", 2.0 )
                , ( "Name2", 12.0 )
                ]
              )
            ]
    in
    describe "Test reshape SeriesPairs with one name"
        [ test "reshape sample" <|
            \_ -> transpose sp1 |> Expect.equal sp2
        , test "reshape sample with more than one name" <|
            \_ -> transpose sp3 |> Expect.equal sp4
        ]


testToMatrix : Test
testToMatrix =
    let
        sp3 =
            [ ( "Name1"
              , [ ( "Label1", 1.0 )
                , ( "Label2", 2.0 )
                ]
              )
            , ( "Name2"
              , [ ( "Label1", 11.0 )
                , ( "Label2", 12.0 )
                ]
              )
            ]
    in
    describe "Test toMatrix"
        [ test "toMatrix sample" <|
            \_ ->
                toMatrix sp3
                    |> Expect.equal [ [ 1.0, 2.0 ], [ 11.0, 12.0 ] ]
        ]


testProjectRelative : Test
testProjectRelative =
    let
        pairs =
            [ ( "a", [ ( "1", 1.0 ), ( "2", 2.0 ), ( "3", 3.0 ) ] )
            , ( "b", [ ( "1", 99.0 ), ( "2", 98.0 ), ( "3", 97.0 ) ] )
            ]

        expectA xs =
            List.map2 (\a b -> abs (a - b) < 0.00001) [ 1.0, 2.0, 3.0 ] xs

        expectB xs =
            List.map2 (\a b -> abs (a - b) < 0.00001) [ 99.0, 98.0, 97.0 ] xs
    in
    describe "Test projectRelative"
        [ test "sample SeriesPairs..." <|
            \_ ->
                projectRelative pairs
                    |> List.map
                        (\( name, xs ) ->
                            if name == "a" then
                                expectA (Utils.snds xs)

                            else
                                expectB (Utils.snds xs)
                        )
                    |> Expect.equal
                        [ [ True, True, True ]
                        , [ True, True, True ]
                        ]
        ]


testSumSeries : Test
testSumSeries =
    let
        pairs =
            [ ( "a", [ ( "1", 1.0 ), ( "2", 2.0 ), ( "3", 3.0 ) ] )
            , ( "b", [ ( "1", 99.0 ), ( "2", 98.0 ), ( "3", 97.0 ) ] )
            ]
    in
    describe "Test sumSeries"
        [ test "sample series" <|
            \_ ->
                sumSeries pairs
                    |> List.map2 (\a b -> abs (a - b) < 0.0001) [ 100.0, 100.0, 100.0 ]
                    |> Expect.equal [ True, True, True ]
        ]


testProjectFirstDeriv : Test
testProjectFirstDeriv =
    describe "Test projectFirstDeriv"
        [ test "delta 1" <|
            \_ ->
                projectFirstDeriv
                    [ ( "series"
                      , [ ( 'a', 1.0 )
                        , ( 'b', 2.0 )
                        , ( 'c', 3.0 )
                        ]
                      )
                    ]
                    |> Expect.equal
                        [ ( "series"
                          , [ ( 'a', 0.0 )
                            , ( 'b', 1.0 )
                            , ( 'c', 1.0 )
                            ]
                          )
                        ]
        ]


testOffsetDelta : Test
testOffsetDelta =
    describe "Test offset delta"
        [ test "delta 1" <|
            \_ ->
                offsetDelta 1
                    [ ( 'a', 1.0 )
                    , ( 'b', 2.0 )
                    , ( 'c', 3.0 )
                    ]
                    |> Expect.equal
                        [ ( 'a', 0.0 )
                        , ( 'b', 1.0 )
                        , ( 'c', 1.0 )
                        ]
        ]
