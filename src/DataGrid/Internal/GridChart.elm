module DataGrid.Internal.GridChart exposing (..)

import Color exposing (Color)
import Scale.Color as ColorScale


--------------------------------------------------------------------------------
-- Axes and Scales


getColor : Float -> Color
getColor f =
    if f >= 0 then
        ColorScale.viridisInterpolator (1 - f)

    else
        ColorScale.plasmaInterpolator (1 - abs f)

