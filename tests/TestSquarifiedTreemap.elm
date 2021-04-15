module TestSquarifiedTreemap exposing (..)

import DataGrid.Internal.SquarifiedTreemap as ST
import DataGrid.Internal.Utils as Utils
import Expect exposing (Expectation)
import List.Nonempty as NE
import Test exposing (..)



--------------------------------------------------------------------------------


approxEqual : Float -> Float -> Bool
approxEqual =
    Utils.approxEqual 0.001



--------------------------------------------------------------------------------


testUpdateOriginAndDims : Test
testUpdateOriginAndDims =
    describe "Test updateOriginDims" <|
        [ test "(0, 0), (2, 10) single area 10" <|
            \_ ->
                ST.updateOriginAndDims
                    { x = 0, y = 0 }
                    { x = 2, y = 10 }
                    (NE.fromElement { area = 10 })
                    |> Expect.equal
                        ( { x = 0, y = 5 }
                        , { x = 2, y = 5 }
                        )
        ]


testPartition : Test
testPartition =
    let
        dims =
            { x = 4, y = 6 }

        areas =
            NE.fromList [ 6, 6, 4, 3, 2, 2, 1 ]
                |> Maybe.withDefault (NE.fromElement 0)
                |> NE.map (\a -> { area = a })

        partitioned =
            NE.Nonempty
                (NE.cons { area = 6 } <| NE.fromElement { area = 6 })
                [ NE.cons { area = 4 } <| NE.fromElement { area = 3 }
                , NE.fromElement { area = 2 }
                , NE.fromElement { area = 2 }
                , NE.fromElement { area = 1 }
                ]
    in
    describe "Partition example with 4 x 6 rectangle" <|
        [ test "areas -> partitioned" <|
            \_ ->
                ST.partition dims areas
                    |> Expect.equal partitioned
        ]


testWorst : Test
testWorst =
    let
        row1 =
            NE.fromElement { area = 6 }

        row2 =
            NE.cons { area = 6 } <| NE.fromElement { area = 6 }
    in
    describe "Example of worst with 4 x 6 rectangle"
        [ test "[6] -> 3 / 2" <|
            \_ ->
                approxEqual (ST.worst row1 4) (8 / 3)
                    |> Expect.equal True
        , test "[6, 6] -> 3 / 2" <|
            \_ ->
                approxEqual (ST.worst row2 4) (3 / 2)
                    |> Expect.equal True
        ]
