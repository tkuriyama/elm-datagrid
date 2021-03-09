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

testSplitPairs : Test
testSplitPairs =
    describe "Test splitPairs"
        [ test "splitPairs" <|
              \_ -> splitPairs [(1, 2), (3, 4)] |> Expect.equal ([1, 3], [2, 4])
        ]
