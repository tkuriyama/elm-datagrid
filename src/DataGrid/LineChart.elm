module DataGrid.LineChart exposing ( render )

{-| Render a a single LineChart with some limited config options.

It should work for most simple use cases, but YMMV; in particular, customizing
axes is better handled by direct interaction with the elm-visualization API.

-}

import Axis
import Color exposing ( Color )
import Scale exposing ( BandScale, ContinuousScale, OrdinalScale
                      , defaultBandConfig )
import Scale.Color
import String.Format
import TypedSvg exposing ( g, circle, style, svg, text_ )
import TypedSvg.Attributes exposing ( alignmentBaseline, class, fill, textAnchor
                                    , transform, viewBox )
import TypedSvg.Attributes.InPx exposing ( cx, cy, height, r, width, x, y )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing ( AlignmentBaseline(..), AnchorAlignment(..)
                               , Paint(..), Transform(..) )

import DataGrid.Config as Cfg
import Internal.Defaults as Defaults
import Internal.Utils as Utils


--------------------------------------------------------------------------------
-- StdChartfg is converted to ChartEnv for internal use

type alias SeriesPair label =
    (String, List (label, Float))

type alias ChartEnv label =
    { w: Float
    , h : Float
    , pad : Cfg.Padding
    , dataScale : ContinuousScale Float
    , labelScale : BandScale label
    , colorScale : OrdinalScale String Color
    , labelShow : Bool
    , labelFmt : label -> String
    , dataTickCt : Int
    , tooltips : Cfg.Tooltips
    , style : String
    }

genChartEnv : Cfg.StdChartCfg label -> List(SeriesPair label) -> ChartEnv label
genChartEnv cfg model =
    let names = List.map Utils.fst model
        xs = List.concatMap (Utils.snd >> Utils.snds) model
        ys = List.map (Utils.snd >> Utils.fsts) model
           |> List.head |> Maybe.withDefault []
    in { w = cfg.w
       , h = cfg.h
       , pad = cfg.pad
       , dataScale = genDataScale cfg.h cfg.pad.bottom xs
       , labelScale = genLabelScale cfg.w cfg.pad.left ys
       , colorScale = genColorScale names 
       , labelShow = cfg.showLabels
       , labelFmt = cfg.labelFormatter
       , dataTickCt = min cfg.dataAxisTicks 10
       , tooltips = cfg.tooltips
       , style = genStyle cfg.fontSpec cfg.chartSpec cfg.tooltips
    }


--------------------------------------------------------------------------------
-- Render

render : Cfg.StdChartCfg label -> List(SeriesPair label) -> Svg msg
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
        , g [ class [ "lines" ]
            , transform [ Translate env.pad.left env.pad.top ] ] <|
            List.map (renderPoints env) model
        , g [ class [ "points" ]
            , transform [ Translate env.pad.left env.pad.top ] ] <|
            List.map (renderLine env) model
        ]

renderPoints : ChartEnv label -> SeriesPair label -> Svg msg
renderPoints env (name, points) =
    let n = List.length points |> toFloat
    in svg [] (List.map (renderPoint env name n) points)

renderPoint : ChartEnv label -> String -> Float -> (label, Float) -> Svg msg
renderPoint env name ct (lbl, val) =
    let rSize = max 3.0 (env.w / ct / 3)
        textX = Scale.convert (Scale.toRenderable env.labelFmt env.labelScale)
                lbl
        e = toFloat (String.length <| env.labelFmt lbl) / 2.0 * 8
        anchor =
            if textX - e < 0 then AnchorStart else
            if textX + e > env.w - env.pad.left * 2 then AnchorEnd else
            AnchorMiddle
    in svg
        []
        [ g [ class [ "bar" ] ]
          [ circle
                [ cx <| Scale.convert env.labelScale lbl
                , cy <| Scale.convert env.dataScale val
                , r rSize
                , fill <| Paint <| getColor env.colorScale name
                ]
                []
            , text_
                [ class [ "tooltip" ]
                , x <| textX
                , y <| env.h - (2 * env.pad.bottom) +
                    (toFloat env.tooltips.tooltipSize)
                , textAnchor <| anchor
                ]
                [ "{{name}} {{lbl}}: {{val}}"
                |> String.Format.namedValue "name" name
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
                [ "{{name}} {{lbl}}: {{val}}"
                |> String.Format.namedValue "name" name
                |> String.Format.namedValue "lbl" (env.labelFmt lbl)
                |> String.Format.namedValue "val" (Utils.fmtFloat 2 val)
                |> text
                ]
            ]
         ]

renderLine : ChartEnv label -> SeriesPair label -> Svg msg
renderLine env model = svg [] []


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

genColorScale : List String -> OrdinalScale String Color
genColorScale names =
    Scale.ordinal Scale.Color.category10 names

getColor : OrdinalScale String Color -> String -> Color
getColor cScale =
    Scale.convert cScale >> Maybe.withDefault Color.black


--------------------------------------------------------------------------------
-- Style

genStyle : Cfg.FontSpec -> Cfg.ChartSpec -> Cfg.Tooltips -> String
genStyle fCfg cCfg tCfg =
    let display b = if b then "inline" else "none"
    in """
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
         |> String.Format.namedValue "textColor" fCfg.textColor
         |> String.Format.namedValue "typeface" fCfg.typeface
