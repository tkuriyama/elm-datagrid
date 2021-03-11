module DataGrid.BarChart exposing ( render )

{-| Render a a single Barchart with some limited config options.

It should work for most simple use cases, but YMMV; in particular, customizing
axes is better handled by direct interaction with the elm-visualization API.

-}

import Axis
import Scale exposing ( BandScale, ContinuousScale, defaultBandConfig )
import String.Format
import TypedSvg exposing ( g, rect, style, svg, text_ )
import TypedSvg.Attributes exposing ( alignmentBaseline, class, textAnchor
                                    , transform, viewBox )
import TypedSvg.Attributes.InPx exposing ( height, width, x, y )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing ( AlignmentBaseline(..), AnchorAlignment(..)
                               , Transform(..) )

import DataGrid.Config as Cfg
import Internal.Defaults as Defaults
import Internal.Utils as Utils


--------------------------------------------------------------------------------
-- StdChartfg is converted to ChartEnv for internal use

type alias ChartEnv label =
    { w: Float
    , h : Float
    , pad : Cfg.Padding
    , dataScale : ContinuousScale Float
    , labelScale : BandScale label
    , labelShow : Bool
    , labelFmt : label -> String
    , dataTickCt : Int
    , tooltips : Cfg.Tooltips
    , style : String
    }

genChartEnv : Cfg.StdChartCfg label -> List (label, Float) -> ChartEnv label
genChartEnv cfg model =
    { w = cfg.w
    , h = cfg.h
    , pad = cfg.pad
    , dataScale = genDataScale cfg.h cfg.pad.bottom <| Utils.snds model
    , labelScale = genLabelScale cfg.w cfg.pad.left <| Utils.fsts model
    , labelShow = cfg.showLabels
    , labelFmt = cfg.labelFormatter
    , dataTickCt = min cfg.dataAxisTicks 10
    , tooltips = cfg.tooltips
    , style = genStyle cfg.fontSpec cfg.chartSpec cfg.tooltips
    }


--------------------------------------------------------------------------------
-- Render

render : Cfg.StdChartCfg label -> List (label, Float) -> Svg msg
render cfg model =
    let env = genChartEnv cfg model
    in svg
        [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g [ class [ "labels" ]
            ,  transform [ Translate (env.pad.left - 1) (env.h - env.pad.bottom) ]
            ]
            [ genLabelAxis env.labelFmt env.labelShow env.labelScale ]
        , g [ class ["dataticks"]
            , transform [ Translate (env.pad.left - 1) env.pad.top ] ]
            [ genDataAxis env.dataTickCt env.dataScale ]
        , g [ class [ "series" ]
            , transform [ Translate env.pad.left env.pad.top ] ] <|
            List.map (barV env) model
        ]

barV : ChartEnv label -> (label, Float) -> Svg msg
barV env (lbl, val) =
    let textX = Scale.convert (Scale.toRenderable env.labelFmt env.labelScale)
                lbl
        e = toFloat (String.length <| env.labelFmt lbl) / 2.0 * 8
        anchor =
            if textX - e < 0 then AnchorStart else
            if textX + e > env.w - env.pad.left * 2 then AnchorEnd else
            AnchorMiddle
    in svg
        []
        [ g [ class [ "bar" ] ]
          [ rect
                [ x <| Scale.convert env.labelScale lbl
                , y <| Scale.convert env.dataScale val
                , width <| Scale.bandwidth env.labelScale
                , height <| env.h -
                    Scale.convert env.dataScale val - (2 * env.pad.bottom)
                ]
                []
            , text_
                [ class [ "tooltip" ]
                , x <| textX
                , y <| env.h - (2 * env.pad.bottom) +
                    (toFloat env.tooltips.tooltipSize)
                , textAnchor <| anchor
                ]
                [ "{{lbl}}: {{val}}"
                |> String.Format.namedValue "lbl" (env.labelFmt lbl)
                |> String.Format.namedValue "val" (Utils.fmtFloat 2 val)
                |> text
                ]
          , text_
                [ class [ "tooltip_large" ]
                , x <| env.pad.left / 2
                , y <| 5
                , textAnchor AnchorStart
                , alignmentBaseline AlignmentHanging
                ]
                [ "{{lbl}}: {{val}}"
                |> String.Format.namedValue "lbl" (env.labelFmt lbl)
                |> String.Format.namedValue "val" (Utils.fmtFloat 2 val)
                |> text
                ]
            ]
        ]


--------------------------------------------------------------------------------
-- Scales and Axes

genDataScale : Float -> Float -> List Float -> ContinuousScale Float
genDataScale end padding xs =
    let dataMin = min 0 (Maybe.withDefault 0 <| List.minimum xs)
        dataMax = Maybe.withDefault 0 <| List.maximum xs
    in Scale.linear (end - 2 * padding, 0) (dataMin, dataMax)

genDataAxis : Int -> ContinuousScale Float -> Svg msg
genDataAxis tickCt dScale =
    Axis.left [ Axis.tickCount tickCt ] dScale

genLabelScale : Float -> Float -> List label -> BandScale label
genLabelScale end padding labels =
    let cfg = { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in Scale.band cfg (0, end - 2 * padding) labels

genLabelAxis : (label -> String) -> Bool -> BandScale label -> Svg msg
genLabelAxis fmt show lScale =
    if show then Axis.bottom [] (Scale.toRenderable fmt lScale)
    else svg [] []


--------------------------------------------------------------------------------
-- Style

genStyle : Cfg.FontSpec -> Cfg.ChartSpec -> Cfg.Tooltips -> String
genStyle fCfg cCfg tCfg =
    let display b = if b then "inline" else "none"
        (fillColor, hoverColor) =
            case cCfg of
                Cfg.BarChartSpec spec -> (spec.fillColor, spec.hoverColor)
                _ -> (defaultFillColor, defaultHoverColor)
    in """
     .bar rect { fill: {{fillColor}}; }
     .bar:hover rect { fill: {{hoverColor}}; }
     .bar .tooltip { display: none; font-size: {{sz}}px; fill: {{textColor}}; }
     .bar .tooltip_large { display: none; font-size: {{szL}}px; 
                           fill: {{textColor}} }
     .bar:hover .tooltip { display: {{showTT}}; }
     .bar:hover .tooltip_large { display: {{showLargeTT}}; }
     text { font-family: {{typeface}}, monospace, sans-serif; }
     """
         |> String.Format.namedValue "showTT" (display tCfg.showTooltips)
         |> String.Format.namedValue "sz" (String.fromInt tCfg.tooltipSize)
         |> String.Format.namedValue "showLargeTT"
            (display tCfg.showLargeTooltips)
         |> String.Format.namedValue "szL"
            (String.fromInt tCfg.largeTooltipSize)
         |> String.Format.namedValue "fillColor" fillColor
         |> String.Format.namedValue "hoverColor" hoverColor
         |> String.Format.namedValue "textColor" fCfg.textColor
         |> String.Format.namedValue "typeface" fCfg.typeface

defaultFillColor : String
defaultFillColor = Defaults.rgbaToString Defaults.defaultFillColor

defaultHoverColor : String
defaultHoverColor = Defaults.rgbaToString Defaults.defaultHoverColor
