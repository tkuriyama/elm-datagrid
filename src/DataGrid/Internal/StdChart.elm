module DataGrid.Internal.StdChart exposing (..)

{-| Functions for working with charts configured by StdChartConfig.
-}

import Axis
import Color exposing (Color)
import DataGrid.Config as Cfg
import DataGrid.Internal.Utils as Utils
import List.Extra as LE
import Scale
    exposing
        ( BandScale
        , ContinuousScale
        , OrdinalScale
        , defaultBandConfig
        )
import Scale.Color
import String.Format
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (alignmentBaseline, class, textAnchor)
import TypedSvg.Attributes.InPx exposing (height, rx, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types
    exposing
        ( AlignmentBaseline(..)
        , AnchorAlignment(..)
        , Paint(..)
        )



--------------------------------------------------------------------------------
-- Scales and Axes


genYScale : Bool -> Float -> Float -> List Float -> ContinuousScale Float
genYScale zeroY h padding xs =
    let
        minY =
            Maybe.withDefault 0 <| List.minimum xs

        dispMin =
            if not zeroY then
                minY

            else
                min 0 minY

        dispMax =
            Maybe.withDefault 0 <| List.maximum xs
    in
    Scale.linear ( h - padding, 0 ) ( dispMin, dispMax )


genYAxis : Int -> ContinuousScale Float -> Svg msg
genYAxis tickCt yScale =
    Axis.left [ Axis.tickCount tickCt ] yScale


genXScale : Float -> Float -> List label -> BandScale label
genXScale w padding labels =
    let
        cfg =
            { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in
    Scale.band cfg ( 0, w - padding ) labels


genXAxis : (label -> String) -> Bool -> BandScale label -> Svg msg
genXAxis fmt show xScale =
    if show then
        Axis.bottom [] (Scale.toRenderable fmt xScale)

    else
        svg [] []


genLegendScale : Float -> Float -> List String -> BandScale String
genLegendScale w padding names =
    let
        cfg =
            { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in
    Scale.band cfg ( 0, w - padding ) names


genColorScale : List String -> OrdinalScale String Color
genColorScale =
    Scale.ordinal Scale.Color.category10


getColor : OrdinalScale String Color -> String -> Color
getColor cScale =
    Scale.convert cScale >> Maybe.withDefault Color.black



--------------------------------------------------------------------------------
-- Tooltips


type alias HasTooltipEnv a label =
    { a
        | w : Float
        , h : Float
        , pad : Cfg.Padding
        , labelScale : BandScale label
        , labelFmt : label -> String
        , tooltips : Cfg.Tooltips
    }


genTooltip : HasTooltipEnv a label -> label -> String -> Svg msg
genTooltip env lbl t =
    let
        textX =
            Scale.convert (Scale.toRenderable env.labelFmt env.labelScale) lbl

        e =
            toFloat (String.length t * env.tooltips.tooltipSize) / 1.5 / 2

        textY =
            env.h
                - env.pad.bottom
                - env.pad.top
                + toFloat env.tooltips.tooltipSize

        anchor =
            if textX - e < env.pad.left then
                AnchorStart

            else if textX + e > env.w - env.pad.left - env.pad.right then
                AnchorEnd

            else
                AnchorMiddle
    in
    text_
        [ class [ "tooltip" ]
        , x <| textX
        , y <| textY
        , textAnchor <| anchor
        ]
        [ text t
        ]


genLargeTooltip : HasTooltipEnv a label -> String -> Svg msg
genLargeTooltip env t =
    text_
        [ class [ "tooltip_large" ]
        , x <| env.pad.left / 2
        , y <| 5
        , textAnchor AnchorStart
        , alignmentBaseline AlignmentHanging
        ]
        [ text t
        ]


type alias HoverEnv =
    { x : Float
    , y : Float
    , h : Float
    , w : Float
    , sortLines : Bool
    }


genHoverTooltipLabelled :
    HasTooltipEnv a label
    -> Bool
    -> label
    -> List ( String, Float )
    -> Svg msg
genHoverTooltipLabelled env sortLines lbl points =
    let
        hEnv =
            genHoverEnv env sortLines (Scale.convert env.labelScale lbl) points

        title =
            env.labelFmt lbl
    in
    renderHoverTooltip env hEnv title points


genHoverTooltipTitled :
    HasTooltipEnv a label
    -> Bool
    -> String
    -> Float
    -> List ( String, Float )
    -> Svg msg
genHoverTooltipTitled env sortLines title hx points =
    let
        hEnv =
            genHoverEnv env sortLines hx points
    in
    renderHoverTooltip env hEnv title points


renderHoverTooltip :
    HasTooltipEnv a label
    -> HoverEnv
    -> String
    -> List ( String, Float )
    -> Svg msg
renderHoverTooltip env hEnv title points =
    let
        pad =
            5

        ( lines, lineParams ) =
            genHoverText env hEnv.sortLines points ( hEnv.x, hEnv.y )
    in
    g [ class [ "tooltip_hover" ] ]
        ([ rect
            [ class [ "invert" ]
            , x hEnv.x
            , y hEnv.y
            , height hEnv.h
            , width hEnv.w
            , rx 3
            ]
            []
         , text_
            [ x <| (hEnv.x + pad)
            , y <| hEnv.y + toFloat env.tooltips.hoverTooltipSize + pad
            ]
            [ text <| title ]
         ]
            ++ List.map2 (renderHoverText pad) lines lineParams
        )


renderHoverText : Float -> String -> ( Float, Float ) -> Svg msg
renderHoverText pad t ( hx, hy ) =
    text_
        [ x <| hx + pad
        , y <| hy + pad
        ]
        [ text t ]


genHoverEnv :
    HasTooltipEnv a label
    -> Bool
    -> Float
    -> List ( String, Float )
    -> HoverEnv
genHoverEnv env sortLines hx pairs =
    let
        xOffset =
            5

        sz =
            env.tooltips.hoverTooltipSize |> toFloat

        hh =
            (List.length pairs + 2 |> toFloat) * sz * 1.03

        hw =
            Utils.fsts pairs
                |> List.map (String.length >> toFloat)
                |> List.maximum
                |> Maybe.withDefault 20
                |> (\n -> (n + 7) * (sz * 0.7))

        hx_ =
            if hx > (env.w - env.pad.right) / 2 then
                hx - hw - xOffset

            else
                hx + xOffset

        hy =
            max 0 (env.h / 2 - hh)
    in
    { x = hx_
    , y = hy
    , w = hw
    , h = hh
    , sortLines = sortLines
    }


genHoverText :
    HasTooltipEnv a label
    -> Bool
    -> List ( String, Float )
    -> ( Float, Float )
    -> ( List String, List ( Float, Float ) )
genHoverText env sortLines pairs ( x0, y0 ) =
    let
        longest =
            Utils.fsts pairs
                |> List.map String.length
                |> List.maximum
                |> Maybe.withDefault 0

        cmp a b =
            compare (Utils.snd a) (Utils.snd b)

        fmt ( s, f ) =
            Utils.twoCols longest 3 s (Utils.fmtFloat 2 f)

        sorted =
            if sortLines then
                List.sortWith cmp pairs |> List.reverse

            else
                pairs

        xs =
            List.repeat (List.length pairs) x0

        ys =
            List.range 2 (List.length pairs + 1)
                |> List.map
                    (\i -> y0 + (i * env.tooltips.hoverTooltipSize |> toFloat) + 5)
    in
    ( sorted |> List.map fmt
    , List.map2 Tuple.pair xs ys
    )



--------------------------------------------------------------------------------
-- Style


genBaseStyle : Cfg.FontSpec -> Cfg.Tooltips -> String
genBaseStyle fCfg tCfg =
    """
     text { font-family: {{typeface}}, monospace, sans-serif; }
     .tooltip { display: none; font-size: {{sz}}px;
     fill: {{textColor}}; }
     .tooltip_large { display: none; font-size: {{szL}}px;
     fill: {{textColor}}; }
     .tooltip_hover { display: none; font-size: {{szH}}px;
     fill: {{textColor}}; }
     .tooltip_hover rect { fill: rgba(250, 250, 250, 1.0); }
     """
        |> String.Format.namedValue "textColor" fCfg.textColor
        |> String.Format.namedValue "sz" (String.fromInt tCfg.tooltipSize)
        |> String.Format.namedValue "szL" (String.fromInt tCfg.largeTooltipSize)
        |> String.Format.namedValue "szH" (String.fromInt tCfg.hoverTooltipSize)
        |> String.Format.namedValue "typeface" fCfg.typeface



--------------------------------------------------------------------------------
-- Projections


transpose : List ( a, List ( b, c ) ) -> List ( b, List ( a, c ) )
transpose pairs =
    let
        names =
            Utils.fsts pairs

        m =
            toMatrix pairs |> LE.transpose

        labels =
            Utils.snds pairs |> List.head |> Maybe.withDefault [] |> Utils.fsts

        f x ys =
            ( x, List.map2 Tuple.pair names ys )
    in
    List.map2 f labels m


toMatrix : List ( a, List ( b, c ) ) -> List (List c)
toMatrix pairs =
    Utils.snds pairs
        |> List.map List.unzip
        |> Utils.snds


projectRelative : Cfg.StdSeriesPairs label -> Cfg.StdSeriesPairs label
projectRelative pairs =
    let
        sums =
            sumSeries pairs
    in
    List.foldr (normalize sums) [] pairs |> List.reverse


sumSeries : Cfg.StdSeriesPairs label -> List Float
sumSeries =
    Utils.snds >> List.map Utils.snds >> LE.transpose >> List.map List.sum


normalize :
    List Float
    -> Cfg.StdSeriesPair label
    -> Cfg.StdSeriesPairs label
    -> Cfg.StdSeriesPairs label
normalize sums ( name, xs ) acc =
    let
        ( labels, nums ) =
            ( Utils.fsts xs, Utils.snds xs )

        nums_ =
            List.map2 (\a b -> a / b * 100) nums sums
    in
    ( name, List.map2 Tuple.pair labels nums_ ) :: acc


projectFirstDeriv : Cfg.StdSeriesPairs label -> Cfg.StdSeriesPairs label
projectFirstDeriv =
    List.map (\( name, pairs ) -> ( name, offsetDelta 1 pairs ))


offsetDelta : Int -> List ( label, Float ) -> List ( label, Float )
offsetDelta i pairs =
    let
        pairs_ =
            List.drop i pairs

        head =
            List.take i pairs |> List.map (\( lbl, _ ) -> ( lbl, 0.0 ))

        f ( _, prev ) ( lbl, next ) =
            ( lbl, next - prev )
    in
    head ++ List.map2 f pairs pairs_


projectSeries :
    List String
    -> Cfg.StdSeriesPairs label
    -> Cfg.StdSeriesPairs label
projectSeries hide =
    List.filter (\( name, _ ) -> List.member name hide |> not)
