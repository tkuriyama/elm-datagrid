module DataGrid.StdChart exposing (..)

{-| Functions for working with charts configured by StdChartConfig.

-}

import Axis
import Color exposing ( Color )
import Scale exposing ( BandScale, ContinuousScale, OrdinalScale
                      , defaultBandConfig )
import Scale.Color
import TypedSvg exposing ( svg, text_ )
import TypedSvg.Attributes exposing ( alignmentBaseline, class, textAnchor)
import TypedSvg.Attributes.InPx exposing ( x, y )
import TypedSvg.Core exposing ( Svg, text )
import TypedSvg.Types exposing ( AlignmentBaseline(..), AnchorAlignment(..) )

import DataGrid.Config as Cfg

--------------------------------------------------------------------------------
-- Scales and Axes

genYScale : Float -> Float -> List Float -> ContinuousScale Float
genYScale h padding xs =
    let dataMin = min 0 (Maybe.withDefault 0 <| List.minimum xs)
        dataMax = Maybe.withDefault 0 <| List.maximum xs
    in Scale.linear (h - padding, 0) (dataMin, dataMax)

genYAxis : Int -> ContinuousScale Float -> Svg msg
genYAxis tickCt yScale =
    Axis.left [ Axis.tickCount tickCt ] yScale

genXScale : Float -> Float -> List label -> BandScale label
genXScale w padding labels =
    let cfg = { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in Scale.band cfg (0, w - padding) labels

genXAxis : (label -> String) -> Bool -> BandScale label -> Svg msg
genXAxis fmt show xScale =
    if show then Axis.bottom [] (Scale.toRenderable fmt xScale)
    else svg [] []

genLegendScale : Float -> Float -> List String -> BandScale String
genLegendScale w padding names =
    let cfg = { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in Scale.band cfg (0, w - padding) names

genColorScale : List String -> OrdinalScale String Color
genColorScale =
    Scale.ordinal Scale.Color.category10

getColor : OrdinalScale String Color -> String -> Color
getColor cScale =
     Scale.convert cScale >> Maybe.withDefault Color.black


--------------------------------------------------------------------------------
-- Tooltips

type alias HasTooltipEnv a label =
    { a |
      w : Float
    , h : Float
    , pad : Cfg.Padding
    , labelScale : BandScale label
    , labelFmt : label -> String
    , tooltips : Cfg.Tooltips
    }

genTooltip : HasTooltipEnv a label -> label -> String -> Svg msg
genTooltip env lbl t =
    let textX =
            Scale.convert (Scale.toRenderable env.labelFmt env.labelScale) lbl
        e =
            toFloat (String.length t * env.tooltips.tooltipSize) / 1.5 / 2
        anchor =
            if textX - e < env.pad.left then
                AnchorStart
            else if textX + e > env.w - env.pad.left - env.pad.right then
                AnchorEnd
            else
                AnchorMiddle
    in text_
        [ class [ "tooltip" ]
        , x <| textX
        , y <| env.h - (2 * env.pad.bottom) +
            (toFloat env.tooltips.tooltipSize)
        , textAnchor <| anchor
        ]
        [ text t
        ]

genLargeTooltip : HasTooltipEnv a label -> String -> Svg msg
genLargeTooltip env t =
    text_
        [ class [ "tooltip_large" ]
        , x <| env.pad.left / 2
        , y <| 5
        , textAnchor AnchorStart
        , alignmentBaseline AlignmentHanging
        ]
        [ text t
        ]
