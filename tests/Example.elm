module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)



--------------------------------------------------------------------------------


suite : Test
suite =
    test "hello world" (\_ -> Expect.equal 4 (2 + 2))
