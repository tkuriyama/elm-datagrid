module TestInternalDefaults exposing (..)

import Element
import Expect exposing (Expectation)
import Test exposing (..)

import Internal.Defaults exposing (..)


--------------------------------------------------------------------------------
-- Color

testRGBToString : Test
testRGBToString =
    describe "Test rgbToString /rgbaToString conversion"
        [ test "64, 64, 64" <|
              \_ -> rgbToString (64, 64, 64) |> Expect.equal "rgb(64, 64, 64)"
        , test "64, 64, 64, 0" <|
              \_ -> rgbaToString (64, 64, 64) 0.7 |>
                    Expect.equal "rgb(64, 64, 64, 0.7)"
        ]

testRGBToElmUI : Test
testRGBToElmUI =
    describe "Test rgbToElmUI conversion"
        [ test "64, 64, 64" <|
              \_ -> rgbToElmUI (64, 64, 64) |>
              Expect.equal (Element.rgb255 64 64 64)
        ]

