module DataGrid.Internal.Utils exposing (..)

import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import List.Extra as LE



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



--------------------------------------------------------------------------------
-- Lists


splitN : Int -> List a -> List (List a)
splitN i xs =
    case List.take i xs of
        [] ->
            []

        ys ->
            ys :: splitN i (List.drop i xs)


toggleMember : a -> List a -> List a
toggleMember x xs =
    if List.member x xs then
        LE.remove x xs

    else
        x :: xs


--------------------------------------------------------------------------------
-- Statistics

mean : List Float -> Float
mean xs = List.sum xs / (toFloat <| List.length xs)


--------------------------------------------------------------------------------
-- Strings


twoCols : Int -> Int -> String -> String -> String
twoCols n extra s1 s2 =
    let
        l1 =
            String.length s1

        spaces =
            String.repeat (n - l1 + extra) nbsp
    in
    String.concat [ s1, spaces, s2 ]


nbsp : String
nbsp =
    String.fromChar (Char.fromCode 160)
