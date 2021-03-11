module DataGrid.BarChart exposing (BarChartConfig, render)

{-| Render a a single Barchart with some limited config options.

It should work for most simple use cases, but YMMV; in particular, customizing
axes is better handled by direct interaction with the elm-visualization API.

-}

import Axis
import DateFormat
import Scale exposing ( BandConfig, BandScale, ContinuousScale
                      , defaultBandConfig )
import String.Format
import Time
import TypedSvg exposing ( g, rect, style, svg, text_ )
import TypedSvg.Attributes exposing ( alignmentBaseline, class, textAnchor
                                    , transform, viewBox )
import TypedSvg.Attributes.InPx exposing ( height, width, x, y )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing ( AlignmentBaseline(..), AnchorAlignment(..)
                               , Transform(..) )

import Internal.Defaults as Defaults
import Internal.Utils as Utils


--------------------------------------------------------------------------------
-- BarChartConfig is converted to ChartEnv for internal use

type alias BarChartConfig label =
    { w : Float
    , h : Float
    , padding : Float
    , dataAxisTicks : Int
    , showLabels : Bool
    , labelFormatter : label -> String
    , tooltipSize : Maybe Int
    , showLargeTooltip : Bool
    , largeTooltipSize : Maybe Int
    , fillColor : Maybe String
    , hoverColor : Maybe String
    , textColor : Maybe String
    , typeface : Maybe String
    }

type alias ChartEnv label =
    { w: Float
    , h : Float
    , pad : Float
    , dataScale : ContinuousScale Float
    , labelScale : BandScale label
    , labelShow : Bool
    , labelFmt : label -> String
    , dataTickCt : Int
    , style : String
    }

genChartEnv : BarChartConfig label -> List (label, Float) -> ChartEnv label
genChartEnv cfg model =
    { w = cfg.w
    , h = cfg.h
    , pad = cfg.padding
    , dataScale = genDataScale cfg.h cfg.padding <| Utils.snds model
    , labelScale = genLabelScale cfg.w cfg.padding <| Utils.fsts model
    , labelShow = cfg.showLabels
    , labelFmt = cfg.labelFormatter
    , dataTickCt = min cfg.dataAxisTicks 10
    , style = genStyle cfg
    }


--------------------------------------------------------------------------------
-- Render

render : BarChartConfig label -> List (label, Float) -> Svg msg
render cfg model =
    let env = genChartEnv cfg model
    in svg
        [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g [ class [ "labels" ]
            ,  transform [ Translate (env.pad - 1) (env.h - env.pad) ]
            ]
            [ genLabelAxis env.labelFmt env.labelShow env.labelScale ]
        , g [ class ["dataticks"]
            , transform [ Translate (env.pad - 1) env.pad ] ]
            [ genDataAxis env.dataTickCt env.dataScale ]
        , g [ class [ "series" ]
            , transform [ Translate env.pad env.pad ] ] <|
            List.map (barV env) model
        ]

barV : ChartEnv label -> (label, Float) -> Svg msg
barV env (lbl, val) =
    let textX = Scale.convert (Scale.toRenderable env.labelFmt env.labelScale)
                lbl
        e = (toFloat (String.length <| env.labelFmt lbl)) / 2.0 * 8
        anchor =
            if textX - e < 0 then AnchorStart else
            if textX + e > (env.w - env.pad * 2) then AnchorEnd else
            AnchorMiddle
    in svg
        []
        [ g [ class [ "bar" ] ]
          [ rect
                [ x <| Scale.convert env.labelScale lbl
                , y <| Scale.convert env.dataScale val
                , width <| Scale.bandwidth env.labelScale
                , height <| env.h -
                    Scale.convert env.dataScale val - (2 * env.pad)
                ]
                []
            , text_
                [ class [ "tooltip" ]
                , x <| textX
                , y <| env.h - (env.pad * 2) + 12
                , textAnchor <| anchor
                ]
                [ "{{lbl}}: {{val}}"
                |> String.Format.namedValue "lbl" (env.labelFmt lbl)
                |> String.Format.namedValue "val" (Utils.fmtFloat 2 val)
                |> text
                ]
          , text_
                [ class [ "tooltip_large" ]
                , x <| env.pad / 2
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
    Axis.left [ Axis.tickCount 5 ] dScale

genLabelScale : Float -> Float -> List label -> BandScale label
genLabelScale end padding labels =
    let cfg = { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in Scale.band cfg (0, end - 2 * padding) labels

genLabelAxis : (label -> String) -> Bool -> BandScale label -> Svg msg
genLabelAxis fmt show lScale =
    case show of
        True -> Axis.bottom [] (Scale.toRenderable fmt lScale)
        False -> svg [] []


--------------------------------------------------------------------------------
-- Style

genStyle : BarChartConfig label -> String
genStyle cfg =
    let showTT = if cfg.showLabels then "none" else "inline"
        sz = Maybe.withDefault 14 cfg.tooltipSize |> String.fromInt
        showLargeTT = if cfg.showLargeTooltip then "inline" else "none"
        szL = Maybe.withDefault 20 cfg.largeTooltipSize |> String.fromInt
        fc = Maybe.withDefault defaultFillColor cfg.fillColor
        hc = Maybe.withDefault defaultHoverColor cfg.hoverColor
        tc = Maybe.withDefault defaultTextColor cfg.textColor
        tf = Maybe.withDefault Defaults.defaultTypeface cfg.typeface
    in """
     .bar rect { fill: {{fc}}; }
     .bar:hover rect { fill: {{hc}}; }
     .bar .tooltip { display: none; font-size: {{sz}}px; fill: {{tc}}; }
     .bar .tooltip_large { display: none; font-size: {{szL}}px; fill: {{tc}} }
     .bar:hover .tooltip { display: {{showTT}}; }
     .bar:hover .tooltip_large { display: {{showLargeTT}}; }
     text { font-family: {{f}}, monospace, sans-serif; }
     """
         |> String.Format.namedValue "showTT" showTT
         |> String.Format.namedValue "sz" sz
         |> String.Format.namedValue "showLargeTT" showLargeTT
         |> String.Format.namedValue "szL" szL
         |> String.Format.namedValue "fc" fc
         |> String.Format.namedValue "hc" hc
         |> String.Format.namedValue "tc" tc
         |> String.Format.namedValue "tf" tf

defaultFillColor : String
defaultFillColor = Defaults.rgbaToString Defaults.defaultFillColor

defaultHoverColor : String
defaultHoverColor = Defaults.rgbaToString Defaults.defaultHoverColor

defaultTextColor : String
defaultTextColor = Defaults.rgbToString Defaults.defaultTextColor
