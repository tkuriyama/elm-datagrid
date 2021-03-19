module DataGrid.BarChart exposing ( render )

{-| Render a a single Barchart with some limited config options.

It should work for most simple use cases, but YMMV; in particular, customizing
axes is better handled by direct interaction with the elm-visualization API.

-}

import Scale exposing ( BandScale, ContinuousScale ) 
import String.Format
import TypedSvg exposing ( g, rect, style, svg, text_ )
import TypedSvg.Attributes exposing ( alignmentBaseline, class, textAnchor
                                    , transform, viewBox )
import TypedSvg.Attributes.InPx exposing ( height, width, x, y )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing ( AlignmentBaseline(..), AnchorAlignment(..)
                               , Transform(..) )

import DataGrid.Config as Cfg
import DataGrid.Internal.Defaults as Defaults
import DataGrid.Internal.Utils as Utils
import DataGrid.StdChart as StdChart


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
    , dataScale = StdChart.genYScale True cfg.h (cfg.pad.top + cfg.pad.bottom)
                  (Utils.snds model)
    , labelScale = StdChart.genXScale cfg.w (cfg.pad.right + cfg.pad.left) <|
                   Utils.fsts model
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
            [ StdChart.genXAxis env.labelFmt env.labelShow env.labelScale ]
        , g [ class ["dataticks"]
            , transform [ Translate (env.pad.left - 1) env.pad.top ] ]
            [ StdChart.genYAxis env.dataTickCt env.dataScale ]
        , g [ class [ "bars" ]
            , transform [ Translate env.pad.left env.pad.top ] ] <|
            List.map (renderBar env) model
        ]

renderBar : ChartEnv label -> (label, Float) -> Svg msg
renderBar env (lbl, val) =
    svg
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
          , "{{lbl}}: {{val}}"
            |> String.Format.namedValue "lbl" (env.labelFmt lbl)
            |> String.Format.namedValue "val" (Utils.fmtFloat 2 val)
            |> StdChart.genTooltip env lbl
          , "{{lbl}}: {{val}}"
            |> String.Format.namedValue "lbl" (env.labelFmt lbl)
            |> String.Format.namedValue "val" (Utils.fmtFloat 2 val)
            |> StdChart.genLargeTooltip env
          ]
        ]


--------------------------------------------------------------------------------
-- Style

genStyle : Cfg.FontSpec -> Cfg.ChartSpec -> Cfg.Tooltips -> String
genStyle fCfg cCfg tCfg =
    let display b = if b then "inline" else "none"
        (fillColor, hoverColor) =
            case cCfg of
                Cfg.BarChartSpec spec -> (spec.fillColor, spec.hoverColor)
                _ -> (defaultFillColor, defaultHoverColor)
    in StdChart.genBaseStyle fCfg tCfg ++
        """
         .bar rect { fill: {{fillColor}}; }
         .bar:hover rect { fill: {{hoverColor}}; }
         .bar:hover .tooltip { display: {{showTT}}; }
         .bar:hover .tooltip_large { display: {{showLargeTT}}; }
         """
         |> String.Format.namedValue "showTT" (display tCfg.showTooltips)
         |> String.Format.namedValue "showLargeTT"
            (display tCfg.showLargeTooltips)
         |> String.Format.namedValue "fillColor" fillColor
         |> String.Format.namedValue "hoverColor" hoverColor

defaultFillColor : String
defaultFillColor = Defaults.rgbaToString Defaults.defaultFillColor

defaultHoverColor : String
defaultHoverColor = Defaults.rgbaToString Defaults.defaultHoverColor

