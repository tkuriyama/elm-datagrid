module DataGrid.StackedBarChart exposing (render)

{-| Render a a single Stacked BarChart with some limited config options.

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
import TypedSvg exposing (g, line, rect, style, svg, text_)
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
        ( height
        , strokeWidth
        , width
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
    , tooltips : Cfg.Tooltips
    , style : String
    }


genChartEnv :
    Cfg.StdChartCfg label
    -> List ( label, List ( String, Float ) )
    -> ChartEnv label
genChartEnv cfg data =
    let
        names =
            List.map (Utils.snd >> Utils.fsts) data
                |> List.head
                |> Maybe.withDefault []

        data_ =
            StdChart.toMatrix data |> List.concatMap Utils.sumRange

        labels =
            Utils.fsts data
    in
    { w = cfg.w
    , h = cfg.h
    , pad = cfg.pad
    , dataScale =
        StdChart.genYScaleWithMax True
            cfg.h
            (cfg.pad.top + cfg.pad.bottom)
            cfg.axesSpec.yAxisMax
            data_
    , labelScale = StdChart.genXScale cfg.w (cfg.pad.right + cfg.pad.left) labels
    , colorScale = StdChart.genColorScale names
    , labelShow = cfg.showLabels
    , labelFmt = cfg.labelFormatter
    , dataTickCt = min cfg.dataAxisTicks 10
    , tooltips = cfg.tooltips
    , style = genStyle cfg.fontSpec cfg.chartSpec cfg.tooltips
    }



--------------------------------------------------------------------------------
-- Render


render : Cfg.StdChartCfg label -> List (Cfg.StdSeries label) -> Svg msg
render cfg data =
    let
        data_ =
            List.sortBy (\( _, pairs ) -> Utils.snds pairs |> List.sum) data
                |> StdChart.transpose

        env =
            genChartEnv cfg data_
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
            [ class [ "stacked_bars" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (List.map (renderStackedBar env) data_)
        , g
            [ class [ "hover_boxes" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (List.map (renderHoverBox env) data_)
        ]


renderStackedBar : ChartEnv label -> ( label, List ( String, Float ) ) -> Svg msg
renderStackedBar env ( lbl, pairs ) =
    let
        zeroY =
            Scale.convert env.dataScale 0

        positive =
            List.filter (\( _, val ) -> val >= 0) pairs

        negative =
            List.filter (\( _, val ) -> val < 0) pairs
    in
    g [ class [ "stacked_bar" ] ] <|
        (List.foldr (renderSubBar env lbl) ( 0, [] ) positive |> Utils.snd)
            ++ (List.foldr (renderSubBar env lbl) ( 0, [] ) negative |> Utils.snd)


renderSubBar :
    ChartEnv label
    -> label
    -> ( String, Float )
    -> ( Float, List (Svg msg) )
    -> ( Float, List (Svg msg) )
renderSubBar env lbl ( name, val ) ( yStart, acc ) =
    let
        zeroY =
            Scale.convert env.dataScale 0

        elem =
            g [ class [ "sub_bar" ] ]
                [ rect
                    [ x <| Scale.convert env.labelScale lbl
                    , y <|
                        if val >= 0 then
                            Scale.convert env.dataScale (yStart + val)

                        else
                            zeroY + yStart
                    , width <| Scale.bandwidth env.labelScale
                    , height <|
                        if val >= 0 then
                            zeroY
                                - Scale.convert env.dataScale val

                        else
                            Scale.convert env.dataScale val
                                - zeroY
                    , fill <| Paint <| StdChart.getColor env.colorScale name
                    ]
                    []
                , "{{name}} {{lbl}}: {{val}}"
                    |> String.Format.namedValue "name" name
                    |> String.Format.namedValue "lbl" (env.labelFmt lbl)
                    |> String.Format.namedValue "val" (Utils.fmtFloat 2 val)
                    |> StdChart.genTooltip env lbl
                , "{{name}} {{lbl}}: {{val}}"
                    |> String.Format.namedValue "name" name
                    |> String.Format.namedValue "lbl" (env.labelFmt lbl)
                    |> String.Format.namedValue "val" (Utils.fmtFloat 2 val)
                    |> StdChart.genLargeTooltip env
                ]
    in
    ( yStart + val, elem :: acc )


renderHoverBox : ChartEnv label -> ( label, List ( String, Float ) ) -> Svg msg
renderHoverBox env ( lbl, points ) =
    g [ class [ "hover_box" ] ]
        [ rect
            [ class [ "invisible" ]
            , x <| Scale.convert env.labelScale lbl
            , y <| 0
            , width <| Scale.bandwidth env.labelScale
            , height <| env.h - env.pad.bottom - env.pad.top
            ]
            []
        , StdChart.genHoverTooltipLabelled env False lbl points
        ]



--------------------------------------------------------------------------------
-- Style


genStyle : Cfg.FontSpec -> Cfg.ChartSpec -> Cfg.Tooltips -> String
genStyle fCfg cCfg tCfg =
    let
        ( showName, nameSize ) =
            case cCfg of
                Cfg.LineChartSpec r ->
                    ( if r.showLineName then
                        "inline"

                      else
                        "none"
                    , r.lineNameSize |> String.fromInt
                    )

                _ ->
                    ( "none", "0px" )
    in
    StdChart.genBaseStyle fCfg tCfg
        ++ """
         .sub_bar:hover .tooltip { display: {{showTT}}; }
         .sub_bar:hover .tooltip_large { display: {{showLargeTT}}; }
         .hover_box .invisible { opacity: 0; }
         .hover_box:hover .invisible { opacity: 0.5; fill: white; }
         .hover_box:hover .tooltip_hover { display: {{showHoverTT}}; }
     """
        |> String.Format.namedValue "showTT" (UI.display tCfg.showTooltips)
        |> String.Format.namedValue "showLargeTT"
            (UI.display tCfg.showLargeTooltips)
        |> String.Format.namedValue "showHoverTT"
            (UI.display tCfg.showHoverTooltips)
