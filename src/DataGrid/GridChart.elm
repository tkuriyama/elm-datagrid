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
        ( strokeWidth
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
    , baseFontSize : Int
    , tooltips : Cfg.Tooltips
    , style : String
    }


genChartEnv : Cfg.GridChartCfg -> List Cfg.GridSeries -> ChartEnv
genChartEnv cfg data =
    let
        xs =
            Utils.snds data |> List.head |> Maybe.withDefault [] |> Utils.fsts

        ys =
            Utils.fsts data
    in
    { w = cfg.w
    , h = cfg.h
    , pad = cfg.pad
    , xScale = genXScale cfg.w (cfg.pad.right + cfg.pad.left) xs
    , yScale = genYScale cfg.h (cfg.pad.top + cfg.pad.bottom) ys
    , dataScales = genDataScales cfg data
    , showHBar = parseChartSpec cfg.chartSpec
    , baseFontSize = cfg.baseFontSize
    , tooltips = cfg.tooltips
    , style = genStyle cfg.fontSpec
    }


genDataScales :
    Cfg.GridChartCfg
    -> List Cfg.GridSeries
    -> List (ContinuousScale Float)
genDataScales cfg data =
    Utils.transposeSeries data
        |> Utils.snds
        |> List.map collect
        |> List.map (genDataScale cfg.h (cfg.pad.top + cfg.pad.bottom))


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


render : Cfg.GridChartCfg -> List Cfg.GridSeries -> Svg msg
render cfg data =
    svg [] []



--------------------------------------------------------------------------------
-- Scales


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


genDataScale : Float -> Float -> List Float -> ContinuousScale Float
genDataScale h padding xs =
    let
        dispMin =
            Maybe.withDefault 0 <| List.minimum xs

        dispMax =
            Maybe.withDefault 0 <| List.maximum xs
    in
    Scale.linear ( h - padding, 0 ) ( dispMin, dispMax )


getColor : Float -> Color
getColor f =
    if f >= 0 then
        ColorScale.viridisInterpolator (1 - f)

    else
        ColorScale.plasmaInterpolator (1 - abs f)



--------------------------------------------------------------------------------


genStyle : Cfg.FontSpec -> String
genStyle fCfg =
    ""
