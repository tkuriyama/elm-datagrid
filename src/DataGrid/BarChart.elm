module DataGrid.BarChart exposing (BarChartConfig, render)

{-| Render a a single Barchart with some limited config options.

It should work for most simple use cases, but YMMV; in particular, customizing
axes is better handled by direct interaction with the elm-visualization API.

-}

import Axis
import DateFormat
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import String.Format
import Time
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))

import Internal.Defaults as Defaults
import Internal.Utils as Utils


--------------------------------------------------------------------------------
-- BarChartConfig is converted to ChartEnv for internal use

type alias BarChartConfig label =
    { w : Float
    , h : Float
    , padding : Float
    , showLabels : Bool
    , showLargeTooltip : Bool
    , labelFormatter : label -> String
    , dataAxisTicks : Int
    , fillColor : Maybe String
    , hoverColor : Maybe String
    , textColor : Maybe String
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
    , style = genStyle cfg.showLabels cfg.fillColor cfg.hoverColor cfg.textColor
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
                , y <| 25
                , textAnchor AnchorStart
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

genStyle : Bool -> Maybe String -> Maybe String -> Maybe String -> String
genStyle showLabels mFillColor mHoverColor mTextColor =
    let fc = Maybe.withDefault defaultFillColor mFillColor
        hc = Maybe.withDefault defaultHoverColor mHoverColor
        tc = Maybe.withDefault defaultTextColor mTextColor
    in defaultStyle showLabels fc hc tc

defaultStyle : Bool -> String -> String -> String -> String
defaultStyle showLabels fillColor hoverColor textColor =
    let showTooltips = if showLabels then "none" else "inline"
    in """
     .bar rect { fill: {{fillColor}}; }
     .bar:hover rect { fill: {{hoverColor}}; }
     .bar .tooltip { display: none; font-size: 12px; fill: {{textColor}}; }
     .bar .tooltip_large { display: none; font-size: 20px; fill: {{textColor}} }
     .bar:hover .tooltip { display: {{showTooltips}}; }
     .bar:hover .tooltip_large { display: {{showTooltips}}; }
     """
         |> String.Format.namedValue "fillColor" fillColor
         |> String.Format.namedValue "hoverColor" hoverColor
         |> String.Format.namedValue "showTooltips" showTooltips
         |> String.Format.namedValue "textColor" textColor

defaultFillColor : String
defaultFillColor = Defaults.rgbaToString Defaults.defaultFillColor

defaultHoverColor : String
defaultHoverColor = Defaults.rgbaToString Defaults.defaultHoverColor

defaultTextColor : String
defaultTextColor = Defaults.rgbToString Defaults.defaultTextColor
