module Internal.Utils exposing (..)

import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)

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


splitPairs : List (a, b) -> (List a, List b)
splitPairs pairs = (fsts pairs, snds pairs)

mapFst : (a -> c) -> List (a, b) -> List (c, b)
mapFst f =
    List.map (\(a, b) -> (f a, b))

mapSnd : (b -> c) -> List (a, b) -> List (a, c)
mapSnd f =
    List.map (\(a, b) -> (a, f b))


--------------------------------------------------------------------------------
-- Formatting

fmtFloat : Int -> Float -> String
fmtFloat dp f =
    let cfg = { usLocale | decimals = Exact dp }
    in FormatNumber.format cfg f


--------------------------------------------------------------------------------
-- Reshaping

reshapeSeriesPairs : List(a, List(b, c)) -> List(b, List(a, c))
reshapeSeriesPairs pairs = []
