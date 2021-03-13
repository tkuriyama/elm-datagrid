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


--------------------------------------------------------------------------------
-- Formatting

fmtFloat : Int -> Float -> String
fmtFloat dp f =
    let cfg = { usLocale | decimals = Exact dp }
    in FormatNumber.format cfg f


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
