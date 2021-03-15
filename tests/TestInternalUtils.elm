module TestInternalUtils exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Internal.Utils exposing (..)


--------------------------------------------------------------------------------
-- Tuples

testFstSnd : Test
testFstSnd =
    describe "Test fst and snd"
        [ test "fst" <|
              \_ -> fst (1, 2) |> Expect.equal 1
        , test "snd" <|
              \_ -> snd (1, 2) |> Expect.equal 2
        ]

testFstsSnds : Test
testFstsSnds =
    describe "Test fsts and snds"
        [ test "fsts" <|
              \_ -> fsts [(1, 2), (3, 4)] |> Expect.equal [1, 3]
        , test "snds" <|
              \_ -> snds [(1, 2), (3, 4)] |> Expect.equal [2, 4]
        ]

testMapFstSnd : Test
testMapFstSnd =
    let f n = n * 2
        pairs = [(1, 2), (3, 4)]
    in describe "Test mapFst and mapSnd"
        [ test "mapFst" <|
              \_ -> mapFst f pairs |> Expect.equal [(2, 2), (6, 4)]
        , test "mapSnd" <|
              \_ -> mapSnd f pairs |> Expect.equal [(1, 4), (3, 8)]
        ]

--------------------------------------------------------------------------------
-- Formatting

testFmtFloat : Test
testFmtFloat =
    describe "Test float formatting"
        [ test "0 dp" <|
              \_ -> fmtFloat 0 0.1234 |> Expect.equal "0"
        , test "3 dp" <|
              \_ -> fmtFloat 3 0.1234 |> Expect.equal "0.123"
        ]


--------------------------------------------------------------------------------
-- Reshaping

testReshapeSeriesPairs : Test
testReshapeSeriesPairs =
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
              \_ -> reshapeSeriesPairs sp1 |> Expect.equal sp2
        ,
            test "reshape sample with more than one name" <|
                \_ -> reshapeSeriesPairs sp3 |> Expect.equal sp4
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

--------------------------------------------------------------------------------
-- Strings

testTwoCols : Test
testTwoCols =
    describe "Test twoCols formatting" <|
        [ test "2 2 a b -> a   b" <|
              \_ -> twoCols 2 2 "a" "b" |>
              Expect.equal ("a" ++ String.repeat 3 nbsp ++ "b")
        ]
