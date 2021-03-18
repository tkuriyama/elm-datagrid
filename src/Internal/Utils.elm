module Internal.Utils exposing (..)

import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import List.Extra as LE


--------------------------------------------------------------------------------
-- Tuples

fst : (a, b) -> a
fst (a, _) = a

fsts : List (a, b) -> List a
fsts = List.map fst


snd : (a, b) -> b
snd (_, b) = b

snds : List (a, b) -> List b
snds = List.map snd

mapFst : (a -> c) -> List (a, b) -> List (c, b)
mapFst f =
    List.map (\(a, b) -> (f a, b))

mapSnd : (b -> c) -> List (a, b) -> List (a, c)
mapSnd f =
    List.map (\(a, b) -> (a, f b))

triple : a -> b -> c -> (a, b, c)
triple a b c = (a, b, c)


--------------------------------------------------------------------------------
-- Formatting

fmtFloat : Int -> Float -> String
fmtFloat dp f =
    let cfg = { usLocale | decimals = Exact dp }
    in FormatNumber.format cfg f


--------------------------------------------------------------------------------
-- Lists

splitN : Int -> List a -> List (List a)
splitN i xs =
  case List.take i xs of
    [] -> []
    ys -> ys :: splitN i (List.drop i xs)


--------------------------------------------------------------------------------
-- Reshaping

-- Essentially a matrix transpose of a x b -> b x a
reshapeSeriesPairs : List (a, List (b, c)) -> List (b, List (a, c))
reshapeSeriesPairs pairs =
    let names = fsts pairs
        m = toMatrix pairs |> LE.transpose
        labels = snds pairs |> List.head |> Maybe.withDefault [] |> fsts
        f x ys = (x, List.map2 Tuple.pair names ys)
    in List.map2 f labels m

toMatrix : List (a, List (b, c)) -> List (List c)
toMatrix pairs =
    snds pairs                    -- List (List (b, c))
        |> List.map List.unzip    -- List (List b, List c)
        |> snds                   -- List (List c)


--------------------------------------------------------------------------------
-- Strings

twoCols : Int -> Int -> String -> String -> String
twoCols n extra s1 s2 =
    let l1 = String.length s1
        l2 = String.length s2
        spaces = String.repeat (n - l1 + extra) nbsp
    in String.concat [s1, spaces, s2]

nbsp : String
nbsp = String.fromChar (Char.fromCode 160)
