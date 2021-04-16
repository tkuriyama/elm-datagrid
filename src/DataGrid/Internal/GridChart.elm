module DataGrid.Internal.GridChart exposing (..)

import Color exposing (Color)
import DataGrid.ChartConfig as Cfg
import Scale.Color as ColorScale


--------------------------------------------------------------------------------
-- Axes and Scales


getColor : Float -> Color
getColor f =
    if f >= 0 then
        ColorScale.viridisInterpolator (1 - f)

    else
        ColorScale.plasmaInterpolator (1 - abs f)


--------------------------------------------------------------------------------
-- Projections

projectSeries :
    List String
    -> List (Cfg.GridSeries a)
    -> List (Cfg.GridSeries a)
projectSeries hide =
    List.filter (\( name, _ ) -> List.member name hide |> not)
