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
-- Lists

testSplitN : Test
testSplitN  =
    describe "Test splitN"
        [ test "well-formed data" <|
              \_ -> splitN 2 [1, 2, 3, 4] |> Expect.equal [[1, 2], [3, 4]]
        , test "not so well-formed but valid data" <|
              \_ -> splitN 2 [1, 2, 3, 4, 5] |>
                    Expect.equal [[1, 2], [3, 4], [5]]
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
