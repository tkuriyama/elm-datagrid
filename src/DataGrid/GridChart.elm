module DataGrid.GridChart exposing (render)

{-| Render a a single Grid Chart.
-}

import Axis
import Color exposing (Color)
import DataGrid.Config as Cfg
import DataGrid.Internal.Utils as Utils
import List.Extra as LE
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Scale.Color as ColorScale
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
        , rx
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


type alias ChartEnv =
    { w : Float
    , h : Float
    , pad : Cfg.Padding
    , xScale : BandScale String
    , yScale : BandScale String
    , dataScales : List (ContinuousScale Float)
    , showHBar : Bool
    , tooltips : Cfg.Tooltips
    , style : String
    }


genChartEnv : Cfg.GridChartCfg -> List Cfg.GridSeries -> ChartEnv
genChartEnv cfg data =
    let
        xs =
            "Labels" ::
                ( Utils.snds data
                |> List.head
                |> Maybe.withDefault []
                |> Utils.fsts
                )

        ys =
            Utils.fsts data

        xScale =
            genXScale cfg.w (cfg.pad.right + cfg.pad.left) xs
    in
    { w = cfg.w
    , h = cfg.h
    , pad = cfg.pad
    , xScale = xScale
    , yScale = genYScale cfg.h (cfg.pad.top + cfg.pad.bottom) ys
    , dataScales = genDataScales data (Scale.bandwidth xScale)
    , showHBar = parseChartSpec cfg.chartSpec
    , tooltips = cfg.tooltips
    , style = genStyle cfg.baseFontSize cfg.fontSpec cfg.tooltips
    }


genDataScales :
    List Cfg.GridSeries
    -> Float
    -> List (ContinuousScale Float)
genDataScales data w =
    Utils.transposeSeries data
        |> Utils.snds
        |> List.map collect
        |> List.map (genDataScale (w / 2))


collect : List ( Cfg.SeriesName, List Cfg.GridPair ) -> List Float
collect pairs =
    Utils.snds pairs
        |> List.map (LE.last >> Maybe.withDefault ( "", 0 ))
        |> Utils.snds


parseChartSpec : Cfg.ChartSpec -> Bool
parseChartSpec spec =
    case spec of
        Cfg.GridChartSpec s ->
            s.showHBar

        _ ->
            False



--------------------------------------------------------------------------------
-- Render

render : Cfg.GridChartCfg -> List Cfg.GridSeries -> Svg msg
render cfg data =
    let
        env =
            genChartEnv cfg data
    in

    svg
        [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g
            [ class [ "grid_labels" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            ( List.map (renderLabel env) <| Utils.fsts data )
        , g
            [ class [ "grids" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            ( List.concatMap (renderGrid env) data )
        ]


renderLabel : ChartEnv -> String -> Svg msg
renderLabel env lbl =
    text_
        [ x <| Scale.convert env.xScale "labels"
        , y <| Scale.convert env.yScale lbl ]
        [ text lbl ]


renderGrid : ChartEnv -> Cfg.GridSeries -> List (Svg msg)
renderGrid env (lbl, groups) =
    let
        y =
            Scale.convert env.yScale lbl
        f group =
            g [ class [ "grid_cells" ] ]
              [ renderCells env y group ]

    in
        List.map f groups

renderCells :
    ChartEnv
    -> Float
    -> (String, List Cfg.GridPair)
    -> Svg msg
renderCells env y (name, pairs) =
    let
        x =
            Scale.convert env.xScale name
        xDiv =
            if env.showHBar then 2.0 else 1.0

        xInc =
            (Scale.bandwidth env.xScale) / xDiv / (List.length pairs |> toFloat)

        f colorVal (xStart, acc) =
            (xStart + xInc, (renderCell xStart y xInc colorVal) :: acc)
    in
        g
        [ ]
        ( relativeScale pairs
        |> List.foldl f (x, [])
        |> Utils.snd
        |> List.reverse
        )

renderCell : Float -> Float -> Float -> Float -> Svg msg
renderCell x_ y_ w colorVal =
    rect
        [ x x_
        , y y_
        , rx 1
        , width w
        , height w
        , fill <| Paint <| getColor colorVal
        ]
        []


relativeScale : List (a, Float) -> List Float
relativeScale xs =
    let f val (scalar, acc) =
            (scalar, ((val - scalar) / scalar) :: acc)
    in 
    case (Utils.snds xs) of
        [] ->
            []
        (y :: ys) ->
            List.foldl f (y, [0]) ys |> Utils.snd |> List.reverse

--------------------------------------------------------------------------------
-- Axes and Scales


genXScale : Float -> Float -> List String -> BandScale String
genXScale w padding xs =
    let
        cfg =
            { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in
    Scale.band cfg ( 0, w - padding ) xs


genXAxis : Bool -> BandScale String -> Svg msg
genXAxis show xScale =
    if show then
        Axis.bottom [] (Scale.toRenderable identity xScale)

    else
        svg [] []


genYScale : Float -> Float -> List String -> BandScale String
genYScale h padding ys =
    let
        cfg =
            { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in
    Scale.band cfg ( h - padding, 0 ) ys


genYAxis : Bool -> BandScale String -> Svg msg
genYAxis show yScale =
    if show then
        Axis.left [] (Scale.toRenderable identity yScale)

    else
        svg [] []


genDataScale : Float -> List Float -> ContinuousScale Float
genDataScale w xs =
    let
        dispMin =
            Maybe.withDefault 0 <| List.minimum xs

        dispMax =
            Maybe.withDefault 0 <| List.maximum xs
    in
    Scale.linear ( 0, w ) ( dispMin, dispMax )


getColor : Float -> Color
getColor f =
    if f >= 0 then
        ColorScale.viridisInterpolator (1 - f)

    else
        ColorScale.plasmaInterpolator (1 - abs f)



--------------------------------------------------------------------------------


genStyle : Int -> Cfg.FontSpec -> Cfg.Tooltips -> String
genStyle sz fCfg tCfg =
    let
        base =
            genBaseStyle sz fCfg tCfg
    in
        base

genBaseStyle : Int -> Cfg.FontSpec -> Cfg.Tooltips -> String
genBaseStyle sz fCfg tCfg =
    """
     text { font-family: {{typeface}}, monospace, sans-serif;
            fill: {{textColor}}; }
     .grid_labels { font-size: {{sz}}px; }
     .tooltip_hover { display: none; font-size: {{szH}}px;
     .tooltip_hover rect { fill: rgba(250, 250, 250, 1.0); }
     """
        |> String.Format.namedValue "sz" (String.fromInt sz)
        |> String.Format.namedValue "textColor" fCfg.textColor
        |> String.Format.namedValue "szH" (String.fromInt tCfg.hoverTooltipSize)
        |> String.Format.namedValue "typeface" fCfg.typeface


