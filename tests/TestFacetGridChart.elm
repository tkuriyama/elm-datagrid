module TestFacetGridChart exposing (..)

import DataGrid.FacetGridChart exposing (..)
import DataGrid.Internal.Utils as Utils
import Expect exposing (Expectation)
import Test exposing (..)



--------------------------------------------------------------------------------
-- Sorting


testSort : Test
testSort =
    let
        series =
            [ ( "a"
              , [ ( "fst"
                  , [ ( "foo", 100.1 ), ( "bar", 1 ) ]
                  )
                ]
              )
            , ( "b"
              , [ ( "fst"
                  , [ ( "foo", 50.1 ), ( "bar", 50 ) ]
                  )
                ]
              )
            , ( "c"
              , [ ( "fst"
                  , [ ( "foo", 0.1 ), ( "bar", 100 ) ]
                  )
                ]
              )
            ]
    in
    describe "test GridSeries sorting"
        [ test "test sorted keys -> c, b, a" <|
            \_ ->
                sortByRecent series
                    |> Utils.fsts
                    |> Expect.equal [ "c", "b", "a" ]
        ]
