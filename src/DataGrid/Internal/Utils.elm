module DataGrid.Internal.Utils exposing (..)

import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import List.Extra as LE
import List.Nonempty as NE
import MapAccumulate



--------------------------------------------------------------------------------
-- Tuples


fst : ( a, b ) -> a
fst ( a, _ ) =
    a


fsts : List ( a, b ) -> List a
fsts =
    List.map fst


snd : ( a, b ) -> b
snd ( _, b ) =
    b


snds : List ( a, b ) -> List b
snds =
    List.map snd


mapFst : (a -> c) -> List ( a, b ) -> List ( c, b )
mapFst f =
    List.map (\( a, b ) -> ( f a, b ))


mapSnd : (b -> c) -> List ( a, b ) -> List ( a, c )
mapSnd f =
    List.map (\( a, b ) -> ( a, f b ))


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )



--------------------------------------------------------------------------------
-- Formatting


fmtFloat : Int -> Float -> String
fmtFloat dp f =
    let
        cfg =
            { usLocale | decimals = Exact dp }
    in
    FormatNumber.format cfg f


approxEqual : Float -> Float -> Float -> Bool
approxEqual epsilon a b =
    abs (a - b) <= epsilon



--------------------------------------------------------------------------------
-- Lists


splitN : Int -> List a -> List (List a)
splitN i xs =
    case List.take i xs of
        [] ->
            []

        ys ->
            ys :: splitN i (List.drop i xs)


transposeSeries : List ( a, List ( b, c ) ) -> List ( b, List ( a, c ) )
transposeSeries pairs =
    let
        names =
            fsts pairs

        m =
            toMatrix pairs |> LE.transpose

        labels =
            snds pairs |> List.head |> Maybe.withDefault [] |> fsts

        f x ys =
            ( x, List.map2 Tuple.pair names ys )
    in
    List.map2 f labels m


toMatrix : List ( a, List ( b, c ) ) -> List (List c)
toMatrix pairs =
    snds pairs
        |> List.map List.unzip
        |> snds


toggleMember : a -> List a -> List a
toggleMember x xs =
    if List.member x xs then
        LE.remove x xs

    else
        x :: xs


sumRange : List Float -> List Float
sumRange xs =
    let
        positive =
            List.filter ((>=) 0) xs |> List.sum

        negative =
            List.filter ((<) 0) xs |> List.sum
    in
    [ positive, negative ]



--------------------------------------------------------------------------------
-- Non-Empty Lists


neMapAccumL :
    (a -> acc -> ( b, acc ))
    -> acc
    -> NE.Nonempty a
    -> ( NE.Nonempty b, acc )
neMapAccumL f z ne =
    case ne of
        NE.Nonempty x xs ->
            let
                ( y, z_ ) =
                    f x z

                ( ys, z__ ) =
                    MapAccumulate.mapAccumL f z_ xs
            in
            ( NE.Nonempty y ys, z__ )



--------------------------------------------------------------------------------
-- Statistics


mean : List Float -> Float
mean xs =
    List.sum xs / (toFloat <| List.length xs)



--------------------------------------------------------------------------------
-- Strings


twoCols : Int -> Int -> String -> String -> String
twoCols leftCol extra s1 s2 =
    let
        l1 =
            String.length s1

        spaces =
            String.repeat (leftCol - l1 + extra) nbsp
    in
    String.concat [ s1, spaces, s2 ]


nbsp : String
nbsp =
    String.fromChar (Char.fromCode 160)


alignRight : List String -> List String
alignRight xs =
    let
        m =
            List.map String.length xs
                |> List.maximum
                |> Maybe.withDefault 0

        padLeft s =
            String.repeat (m - String.length s) nbsp ++ s
    in
    List.map padLeft xs


pairWidthMax : List ( String, Float ) -> Int -> Int
pairWidthMax pairs dp =
    let
        maxLength =
            List.map String.length >> List.maximum >> Maybe.withDefault 0

        labelMax =
            pairs |> fsts |> maxLength

        dataMax =
            pairs |> snds |> List.map (fmtFloat dp) |> maxLength
    in
    labelMax + dataMax
