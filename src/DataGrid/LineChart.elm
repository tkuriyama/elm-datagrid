module DataGrid.LineChart exposing (render)

{-| Render a a single LineChart with some limited config options.

It should work for most simple use cases, but YMMV; in particular, customizing
axes is better handled by direct interaction with the elm-visualization API.

-}

import Color exposing (Color)
import DataGrid.ChartConfig as Cfg
import DataGrid.Internal.StdChart as StdChart
import DataGrid.Internal.UI as UI
import DataGrid.Internal.Utils as Utils
import List.Extra as LE
import Path
import Scale exposing (BandScale, ContinuousScale, OrdinalScale)
import Shape
import String.Format
import TypedSvg exposing (circle, g, line, style, svg, text_)
import TypedSvg.Attributes
    exposing
        ( class
        , fill
        , stroke
        , textAnchor
        , transform
        , viewBox
        )
import TypedSvg.Attributes.InPx
    exposing
        ( cx
        , cy
        , r
        , strokeWidth
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types
    exposing
        ( AlignmentBaseline(..)
        , AnchorAlignment(..)
        , Paint(..)
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
    , colorScale : OrdinalScale String Color
    , labelShow : Bool
    , labelFmt : label -> String
    , dataTickCt : Int
    , lineThickness : Float
    , showYZeroLine : Bool
    , tooltips : Cfg.Tooltips
    , style : String
    }


genChartEnv : Cfg.StdChartCfg label -> List (Cfg.StdSeries label) -> ChartEnv label
genChartEnv cfg data =
    let
        names =
            List.map Utils.fst data

        xs =
            List.concatMap (Utils.snd >> Utils.snds) data

        ys =
            List.map (Utils.snd >> Utils.fsts) data
                |> List.head
                |> Maybe.withDefault []

        ( showVBar, lineThickness, showYZeroLine ) =
            parseChartSpec cfg.chartSpec
    in
    { w = cfg.w
    , h = cfg.h
    , pad = cfg.pad
    , dataScale =
        StdChart.genYScale False
            cfg.h
            (cfg.pad.top + cfg.pad.bottom)
            xs
    , labelScale = StdChart.genXScale cfg.w (cfg.pad.right + cfg.pad.left) ys
    , colorScale = StdChart.genColorScale names
    , labelShow = cfg.showLabels
    , labelFmt = cfg.labelFormatter
    , dataTickCt = min cfg.dataAxisTicks 10
    , lineThickness = lineThickness
    , showYZeroLine = showYZeroLine
    , tooltips = cfg.tooltips
    , style =
        genStyle cfg.fontSpec cfg.chartSpec cfg.tooltips showVBar
    }


parseChartSpec : Cfg.ChartSpec -> ( Bool, Float, Bool )
parseChartSpec spec =
    case spec of
        Cfg.LineChartSpec r ->
            ( r.showVBar, r.lineThickness, r.showYZeroLine )

        _ ->
            ( False, 1, False )



--------------------------------------------------------------------------------
-- Render


render : Cfg.StdChartCfg label -> List (Cfg.StdSeries label) -> Svg msg
render cfg data =
    let
        env =
            genChartEnv cfg data

        data_ =
            Utils.transposeSeries data
    in
    svg
        [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g
            [ class [ "labels" ]
            , transform
                [ Translate (env.pad.left - 1)
                    (env.h - env.pad.bottom)
                ]
            ]
            [ StdChart.genXAxis env.labelFmt env.labelShow env.labelScale ]
        , g
            [ class [ "dataticks" ]
            , transform [ Translate (env.pad.left - 1) env.pad.top ]
            ]
            [ StdChart.genYAxis env.dataTickCt env.dataScale ]
        , g
            [ class [ "zeroYLine" ]
            , transform [ Translate 0 env.pad.top ]
            ]
            [ renderZeroYLine env ]
        , g
            [ class [ "points" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
          <|
            List.map (renderPoints env) data
        , g
            [ class [ "lines" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
          <|
            List.map (renderLine env) data
        , g
            [ class [ "vbars" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
          <|
            List.map (renderVBarHover env) data_
        ]


renderZeroYLine : ChartEnv label -> Svg msg
renderZeroYLine env =
    if env.showYZeroLine then
        g [ class [ "zeroLine" ] ]
            [ line
                [ x1 <| env.pad.left
                , x2 <| env.w - env.pad.left
                , y1 <| Scale.convert env.dataScale 0
                , y2 <| Scale.convert env.dataScale 0
                , strokeWidth 1
                , stroke <| Paint <| Color.rgb 0.5 0.5 0.5
                ]
                []
            ]

    else
        g [] []


renderLine : ChartEnv label -> Cfg.StdSeries label -> Svg msg
renderLine env ( name, points ) =
    let
        f ( x, y ) =
            Just
                ( Scale.convert env.labelScale x
                , Scale.convert env.dataScale y
                )

        path =
            Shape.line Shape.monotoneInXCurve <| List.map f points

        lastY =
            Utils.snds points
                |> LE.last
                |> Maybe.withDefault 0.0
                |> Scale.convert env.dataScale
    in
    g [ class [ "line" ] ]
        [ Path.element
            path
            [ stroke <| Paint <| StdChart.getColor env.colorScale name
            , strokeWidth env.lineThickness
            , fill PaintNone
            ]
        , text_
            [ class [ "name" ]
            , x <| env.w - env.pad.left - env.pad.right
            , y <| lastY
            , textAnchor <| AnchorStart
            , fill <| Paint <| StdChart.getColor env.colorScale name
            ]
            [ "{{name}}"
                |> String.Format.namedValue "name" name
                |> text
            ]
        ]


renderPoints : ChartEnv label -> Cfg.StdSeries label -> Svg msg
renderPoints env ( name, points ) =
    let
        n =
            List.length points |> toFloat
    in
    svg [] (List.map (renderPoint env name n) points)


renderPoint : ChartEnv label -> String -> Float -> ( label, Float ) -> Svg msg
renderPoint env name ct ( lbl, val ) =
    let
        rSize =
            min (env.lineThickness * 1.75) (env.w / ct / 4)
    in
    g [ class [ "point" ] ]
        [ circle
            [ cx <| Scale.convert env.labelScale lbl
            , cy <| Scale.convert env.dataScale val
            , r rSize
            , fill <| Paint <| StdChart.getColor env.colorScale name
            ]
            []
        , "{{name}} {{lbl}}: {{val}}"
            |> String.Format.namedValue "name" name
            |> String.Format.namedValue "lbl" (env.labelFmt lbl)
            |> String.Format.namedValue "val"
                (Utils.fmtFloat env.tooltips.floatDp val)
            |> StdChart.genTooltip env lbl
        , "{{name}} {{lbl}}: {{val}}"
            |> String.Format.namedValue "name" name
            |> String.Format.namedValue "lbl" (env.labelFmt lbl)
            |> String.Format.namedValue "val"
                (Utils.fmtFloat env.tooltips.floatDp val)
            |> StdChart.genLargeTooltip env
        ]


renderVBarHover : ChartEnv label -> ( label, List ( String, Float ) ) -> Svg msg
renderVBarHover env ( lbl, points ) =
    let
        lineX =
            Scale.convert env.labelScale lbl
    in
    g [ class [ "vbar" ] ]
        [ line
            [ x1 <| lineX
            , x2 <| lineX
            , y1 <| 0
            , y2 <| env.h - env.pad.top - env.pad.bottom
            , strokeWidth 2
            , stroke <| Paint <| Color.rgb 0.5 0.5 0.5
            ]
            []
        , StdChart.genHoverTooltipLabelled env True lbl points
        ]



--------------------------------------------------------------------------------
-- Style


genStyle : Cfg.FontSpec -> Cfg.ChartSpec -> Cfg.Tooltips -> Bool -> String
genStyle fCfg cCfg tCfg vbar =
    let
        ( showName, nameSize, lineThickness ) =
            case cCfg of
                Cfg.LineChartSpec r ->
                    ( if r.showLineName then
                        "inline"

                      else
                        "none"
                    , r.lineNameSize |> String.fromInt
                    , max 4 (r.lineThickness * 2 |> round) |> String.fromInt
                    )

                _ ->
                    ( "none", "0", "4" )
    in
    StdChart.genBaseStyle fCfg tCfg
        ++ """
         .point:hover .tooltip { display: {{showTT}}; }
         .point:hover .tooltip_large { display: {{showLargeTT}}; }
         .line:hover path { stroke-width: {{lineThickness}}px; }
         .line:hover text { font-weight: bold; }
         .name { display: {{showName}}; font-size: {{nameSize}}px; }
         .name:hover { font-weight: bold; }
         path { pointer-events: stroke; }
         .vbar line { display: inline; opacity: 0; }
         .vbar:hover line { opacity: {{showVBar}}; }
         .vbar:hover .tooltip_hover { display: {{showHoverTT}}; }
     """
        |> String.Format.namedValue "showTT" (UI.display tCfg.showTooltips)
        |> String.Format.namedValue "showLargeTT"
            (UI.display tCfg.showLargeTooltips)
        |> String.Format.namedValue "showHoverTT"
            (UI.display tCfg.showHoverTooltips)
        |> String.Format.namedValue "showName" showName
        |> String.Format.namedValue "nameSize" nameSize
        |> String.Format.namedValue "lineThickness" lineThickness
        |> String.Format.namedValue "showVBar" (UI.reveal vbar 0.8)
