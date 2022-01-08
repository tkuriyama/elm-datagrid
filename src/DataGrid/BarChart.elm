module DataGrid.BarChart exposing (render)

{-| Render a a single Barchart with some limited config options.

It should work for most simple use cases, but YMMV; in particular, customizing
axes is better handled by direct interaction with the elm-visualization API.

-}

import DataGrid.ChartConfig as Cfg
import DataGrid.Internal.Components as Components
import DataGrid.Internal.Defaults as Defaults
import DataGrid.Internal.StdChart as StdChart
import DataGrid.Internal.UI as UI
import DataGrid.Internal.Utils as Utils
import Scale exposing (BandScale, ContinuousScale)
import Statistics
import String.Format
import TypedSvg exposing (g, rect, style, svg)
import TypedSvg.Attributes exposing (class, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types
    exposing
        ( AlignmentBaseline(..)
        , AnchorAlignment(..)
        , Transform(..)
        )



--------------------------------------------------------------------------------
-- StdChartfg is converted to ChartEnv for internal use


type alias ChartEnv label =
    { w : Float
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


genChartEnv : Cfg.StdChartCfg label -> List ( label, Float ) -> ChartEnv label
genChartEnv cfg data =
    { w = cfg.w
    , h = cfg.h
    , pad = cfg.pad
    , dataScale =
        StdChart.genYScaleWithMax True
            cfg.h
            (cfg.pad.top + cfg.pad.bottom)
            cfg.axesSpec.yAxisMax
            (Utils.snds data)
    , labelScale =
        StdChart.genXScale cfg.w (cfg.pad.right + cfg.pad.left) <|
            Utils.fsts data
    , labelShow = cfg.showLabels
    , labelFmt = cfg.labelFormatter
    , dataTickCt = min cfg.dataAxisTicks 10
    , tooltips = cfg.tooltips
    , style =
        genStyle cfg.fontSpec cfg.chartSpec cfg.tooltips <|
            parseChartSpec cfg.chartSpec
    }


parseChartSpec : Cfg.ChartSpec -> Bool
parseChartSpec spec =
    case spec of
        Cfg.BarChartSpec d ->
            d.showDistribution

        _ ->
            False



--------------------------------------------------------------------------------
-- Render


render : Cfg.StdChartCfg label -> Cfg.StdSeries label -> Svg msg
render cfg ( _, data ) =
    let
        env =
            genChartEnv cfg data
    in
    svg
        [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g
            [ class [ "labels" ]
            , transform [ Translate (env.pad.left - 1) (env.h - env.pad.bottom) ]
            ]
            [ StdChart.genXAxis env.labelFmt env.labelShow env.labelScale ]
        , g
            [ class [ "dataticks" ]
            , transform [ Translate (env.pad.left - 1) env.pad.top ]
            ]
            [ StdChart.genYAxis env.dataTickCt env.dataScale ]
        , g
            [ class [ "bars" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (List.map (renderBar env) data)
        , g
            [ class [ "statistics" ]
            , transform [ Translate 0 env.pad.top ]
            ]
            (renderBoxPlot env <| Utils.snds data)
        ]


renderBar : ChartEnv label -> ( label, Float ) -> Svg msg
renderBar env ( lbl, val ) =
    let
        zeroY =
            Scale.convert env.dataScale 0
    in
    svg
        []
        [ g [ class [ "bar" ] ]
            [ rect
                [ x <| Scale.convert env.labelScale lbl
                , y <|
                    if val >= 0 then
                        Scale.convert env.dataScale val

                    else
                        zeroY
                , width <| Scale.bandwidth env.labelScale
                , height <|
                    if val >= 0 then
                        zeroY
                            - Scale.convert env.dataScale val

                    else
                        Scale.convert env.dataScale val
                            - zeroY
                ]
                []
            , "{{lbl}}: {{val}}"
                |> String.Format.namedValue "lbl" (env.labelFmt lbl)
                |> String.Format.namedValue "val"
                    (Utils.fmtFloat env.tooltips.floatDp val)
                |> StdChart.genTooltip env lbl
            , "{{lbl}}: {{val}}"
                |> String.Format.namedValue "lbl" (env.labelFmt lbl)
                |> String.Format.namedValue "val"
                    (Utils.fmtFloat env.tooltips.floatDp val)
                |> StdChart.genLargeTooltip env
            ]
        ]


renderBoxPlot : ChartEnv label -> List Float -> List (Svg msg)
renderBoxPlot env xs =
    let
        xs_ =
            List.sort xs

        quantile q =
            Statistics.quantile q >> Maybe.withDefault 0

        convert =
            Scale.convert env.dataScale

        ( p25, p50, p75 ) =
            ( quantile 0.25 xs_, quantile 0.5 xs_, quantile 0.75 xs_ )

        bp =
            { x = 10
            , y = 0
            , w = 10
            , p0 = List.minimum xs_ |> Maybe.withDefault 0 |> convert
            , p25 = p25 |> convert
            , p50 = p50 |> convert
            , p75 = p75 |> convert
            , p100 = List.maximum xs_ |> Maybe.withDefault 0 |> convert
            }

        pairs =
            [ ( "25%", p25 )
            , ( "50%", p50 )
            , ( "75%", p75 )
            , ( "Mean", Utils.mean xs )
            , ( "Std", Statistics.deviation xs |> Maybe.withDefault 0 )
            ]

        tooltipX =
            env.w - env.pad.right + bp.x
    in
    [ g [ transform [ Translate (env.w - env.pad.right) 0 ] ]
        [ Components.boxPlot bp ]
    , StdChart.genHoverTooltipTitled env True "Statistics" tooltipX pairs
    ]



--------------------------------------------------------------------------------
-- Style


genStyle : Cfg.FontSpec -> Cfg.ChartSpec -> Cfg.Tooltips -> Bool -> String
genStyle fCfg cCfg tCfg showBoxPlot =
    let
        ( fillColor, hoverColor ) =
            case cCfg of
                Cfg.BarChartSpec spec ->
                    ( spec.fillColor, spec.hoverColor )

                _ ->
                    ( defaultFillColor, defaultHoverColor )
    in
    StdChart.genBaseStyle fCfg tCfg
        ++ """
         .bar rect { fill: {{fillColor}}; }
         .bar:hover rect { fill: {{hoverColor}}; }
         .bar:hover .tooltip { display: {{showTT}}; }
         .bar:hover .tooltip_large { display: {{showLargeTT}}; }
         .statistics { display: {{showBoxPlot}}; }
         .boxplot rect { fill: {{fillColor }}; }
         .statistics .tooltip_hover { display: none; }
         .statistics:hover .tooltip_hover { display: inline; }
         """
        |> String.Format.namedValue "showTT" (UI.display tCfg.showTooltips)
        |> String.Format.namedValue "showLargeTT"
            (UI.display tCfg.showLargeTooltips)
        |> String.Format.namedValue "fillColor" fillColor
        |> String.Format.namedValue "hoverColor" hoverColor
        |> String.Format.namedValue "showBoxPlot" (UI.display showBoxPlot)


defaultFillColor : String
defaultFillColor =
    Defaults.rgbaToString Defaults.defaultFillColor


defaultHoverColor : String
defaultHoverColor =
    Defaults.rgbaToString Defaults.defaultHoverColor
