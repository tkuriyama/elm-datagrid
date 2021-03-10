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


--------------------------------------------------------------------------------
-- Formatting

fmtFloat : Int -> Float -> String
fmtFloat dp f =
    let cfg = { usLocale | decimals = Exact dp }
    in FormatNumber.format cfg f

