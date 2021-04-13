module DataGrid.Internal.SquarifiedTreemap exposing (..)

{-| generate a Squarified Treemap
<https://www.win.tue.nl/~vanwijk/stm.pdf>
-}

import List.Nonempty as NE



--------------------------------------------------------------------------------


type alias HasArea a =
    { a | area : Float }


type alias Dimensions =
    ( Float, Float )



--------------------------------------------------------------------------------


makeDimensions : Float -> Float -> Dimensions
makeDimensions a b =
    if a < b then
        ( a, b )

    else
        ( b, a )


shorterDim : Dimensions -> Float
shorterDim =
    Tuple.first

longerDim : Dimensions -> Float
longerDim =
    Tuple.second


--------------------------------------------------------------------------------


partition : Dimensions -> NE.Nonempty (HasArea a) -> List (Row a)
partition dims areas =
    NE.tail areas
        |> List.foldl partitionHelper ( dims, NE.fromElement (NE.head areas), [] )
        |> (\( _, row, rows ) ->
                row
                    :: rows
                    |> List.map NE.reverse
                    |> List.reverse
           )


type alias Row a =
    NE.Nonempty (HasArea a)


type alias PartitionAcc a =
    ( Dimensions, Row a, List (Row a) )


partitionHelper : HasArea a -> PartitionAcc a -> PartitionAcc a
partitionHelper area ( dims, row, rows ) =
    let
        w =
            shorterDim dims
    in
    if worst row w >= worst (NE.cons area row) w then
        ( dims, NE.cons area row, rows )

    else
        ( updateDims dims row, NE.fromElement area, row :: rows )


updateDims : Dimensions -> Row a -> Dimensions
updateDims dims row =
    let
        (s, l) =
            (shorterDim dims, longerDim dims)

        totalArea =
            NE.map (.area) row |> NE.foldl1 (+)

    in
        makeDimensions (totalArea / s) (l - (totalArea / s))


worst : Row a -> Float -> Float
worst row w =
    let
        areas =
            NE.map .area row

        rowMax =
            NE.foldl1 max areas

        rowMin =
            NE.foldl1 min areas

        rowTotal =
            NE.foldl1 (+) areas
    in
    max (w ^ 2 * rowMax / rowTotal ^ 2) (rowTotal ^ 2 / (w ^ 2 * rowMin))
