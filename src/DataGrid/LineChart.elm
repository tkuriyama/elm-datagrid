module DataGrid.LineChart exposing ( render )

{-| Render a a single LineChart with some limited config options.

It should work for most simple use cases, but YMMV; in particular, customizing
axes is better handled by direct interaction with the elm-visualization API.

-}

import Color exposing ( Color )
import Path exposing ( Path )
import Scale exposing ( BandScale, ContinuousScale, OrdinalScale )
import Shape
import String.Format
import TypedSvg exposing ( g, circle, style, svg, text_ )
import TypedSvg.Attributes exposing ( alignmentBaseline, class, fill, stroke, textAnchor
                                    , transform, viewBox )
import TypedSvg.Attributes.InPx exposing ( cx, cy, height, r, strokeWidth, width, x, y )
import TypedSvg.Core exposing ( Svg, text )
import TypedSvg.Types exposing ( AlignmentBaseline(..), AnchorAlignment(..)
                               , Paint(..), Transform(..) )

import DataGrid.Config as Cfg
import DataGrid.StdChart as StdChart
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
    , showVBar : Bool
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
       , dataScale = StdChart.genYScale cfg.h (cfg.pad.top + cfg.pad.bottom) xs
       , labelScale = StdChart.genXScale cfg.w (cfg.pad.right + cfg.pad.left) ys
       , colorScale = StdChart.genColorScale names
       , labelShow = cfg.showLabels
       , labelFmt = cfg.labelFormatter
       , dataTickCt = min cfg.dataAxisTicks 10
       , tooltips = cfg.tooltips
       , showVBar = parseChartSpec cfg.chartSpec
       , style = genStyle cfg.fontSpec cfg.chartSpec cfg.tooltips
    }

parseChartSpec : Cfg.ChartSpec -> Bool
parseChartSpec spec =
    case spec of
        Cfg.LineChartSpec r -> r.showVBar
        _ -> False


--------------------------------------------------------------------------------
-- Render

render : Cfg.StdChartCfg label -> List(SeriesPair label) -> Svg msg
render cfg model =
    let env = genChartEnv cfg model
        model_ = reshape model
    in svg
        [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g [ class [ "labels" ]
            ,  transform [ Translate (env.pad.left - 1) (env.h - env.pad.bottom) ] ]
            [ StdChart.genXAxis env.labelFmt env.labelShow env.labelScale ]
        , g [ class ["dataticks"]
            , transform [ Translate (env.pad.left - 1) env.pad.top ] ]
            [ StdChart.genYAxis env.dataTickCt env.dataScale ]
        , g [ class [ "lines" ]
            , transform [ Translate env.pad.left env.pad.top ] ] <|
            List.map (renderPoints env) model
        , g [ class [ "points" ]
            , transform [ Translate env.pad.left env.pad.top ] ] <|
            List.map (renderLine env) model
        , if not env.showVBar then g [] [] else
              g [ class [ "vbars" ]
                , transform [ Translate env.pad.left env.pad.top ] ] <|
                List.map (renderVBar env) model_
        ]

renderLine : ChartEnv label -> SeriesPair label -> Svg msg
renderLine env (name, points) =
    let f (x, y) = Just ( Scale.convert env.labelScale x
                        , Scale.convert env.dataScale y )
        path = Shape.line Shape.monotoneInXCurve <| List.map f points
    in svg
        []
        [ g [ class [ "line" ] ]
            [ Path.element
                  path
                  [ stroke <| Paint <| StdChart.getColor env.colorScale name
                  , strokeWidth 1
                  , fill PaintNone
                  ]
            ]
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
        [ g [ class [ "point" ] ]
          [ circle
                [ cx <| Scale.convert env.labelScale lbl
                , cy <| Scale.convert env.dataScale val
                , r rSize
                , fill <| Paint <| StdChart.getColor env.colorScale name
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

renderVBar : ChartEnv label -> (label, List Float) -> Svg msg
renderVBar env (label, points) = svg [] []


--------------------------------------------------------------------------------
-- Style

genStyle : Cfg.FontSpec -> Cfg.ChartSpec -> Cfg.Tooltips -> String
genStyle fCfg cCfg tCfg =
    let display b = if b then "inline" else "none"
    in """
     .point .tooltip { display: none; font-size: {{sz}}px; fill: {{textColor}}; }
     .point .tooltip_large { display: none; font-size: {{szL}}px;
                           fill: {{textColor}} }
     .point:hover .tooltip { display: {{showTT}}; }
     .point:hover .tooltip_large { display: {{showLargeTT}}; }
     .line:hover path { stroke-width: 4px; }
     path { pointer-events: stroke; }
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


--------------------------------------------------------------------------------
-- Helpers

reshape : List(SeriesPair label) -> List(label, List Float)
reshape model = []
