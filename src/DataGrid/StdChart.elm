module DataGrid.StdChart exposing (..)

{-| Functions for working with charts configured by StdChartConfig.

-}

import Axis
import Color exposing ( Color )
import Scale exposing ( BandScale, ContinuousScale, OrdinalScale
                      , defaultBandConfig )
import Scale.Color
import TypedSvg exposing ( svg )
import TypedSvg.Core exposing ( Svg )

--------------------------------------------------------------------------------
-- Scales and Axes

genYScale : Float -> Float -> List Float -> ContinuousScale Float
genYScale end padding xs =
    let dataMin = min 0 (Maybe.withDefault 0 <| List.minimum xs)
        dataMax = Maybe.withDefault 0 <| List.maximum xs
    in Scale.linear (end - padding, 0) (dataMin, dataMax)

genYAxis : Int -> ContinuousScale Float -> Svg msg
genYAxis tickCt yScale =
    Axis.left [ Axis.tickCount tickCt ] yScale

genXScale : Float -> Float -> List label -> BandScale label
genXScale end padding labels =
    let cfg = { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in Scale.band cfg (0, end - padding) labels

genXAxis : (label -> String) -> Bool -> BandScale label -> Svg msg
genXAxis fmt show xScale =
    if show then Axis.bottom [] (Scale.toRenderable fmt xScale)
    else svg [] []

genLegendScale : Float -> Float -> List String -> BandScale String
genLegendScale end padding names =
    let cfg = { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in Scale.band cfg (0, end - padding) names

genColorScale : List String -> OrdinalScale String Color
genColorScale =
    Scale.ordinal Scale.Color.category10

getColor : OrdinalScale String Color -> String -> Color
getColor cScale =
     Scale.convert cScale >> Maybe.withDefault Color.black
