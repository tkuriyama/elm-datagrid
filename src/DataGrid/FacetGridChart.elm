module DataGrid.FacetGridChart exposing (render, sortByRecent)

{-| Render a a single Grid Chart.
-}

import Axis
import Color exposing (Color)
import DataGrid.ChartConfig as Cfg
import DataGrid.Internal.Defaults as Defaults
import DataGrid.Internal.UI as UI
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
        , transform
        , viewBox
        )
import TypedSvg.Attributes.InPx
    exposing
        ( height
        , rx
        , width
        , x
        , y
        )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Paint(..), Transform(..))



--------------------------------------------------------------------------------


type alias ChartEnv =
    { w : Float
    , h : Float
    , cellW : Float
    , cellH : Float
    , pad : Cfg.Padding
    , xScale : BandScale String
    , yScale : BandScale String
    , dataScales : List (ContinuousScale Float)
    , showHBar : Bool
    , labelAlign : Cfg.Position
    , tooltips : Cfg.Tooltips
    , baseFontSize : Int
    , style : String
    , legend : Cfg.Legend
    }


genChartEnv : Cfg.GridChartCfg -> List (Cfg.GridSeries Cfg.GridPair) -> ChartEnv
genChartEnv cfg data =
    let
        ( xs, ys ) =
            getLabels <| sortByRecent data

        xScale =
            genXScale cfg.w (cfg.pad.right + cfg.pad.left) xs

        yScale =
            genYScale cfg.h (cfg.pad.top + cfg.pad.bottom) ys

        ( showHBar_, labelAlign_ ) =
            parseChartSpec cfg.chartSpec

        ( cw, ch ) =
            getDims xScale yScale (List.head data) showHBar_
    in
    { w = cfg.w
    , h = cfg.h
    , cellW = cw
    , cellH = ch
    , pad = cfg.pad
    , xScale = xScale
    , yScale = yScale
    , dataScales = genDataScales data (Scale.bandwidth xScale)
    , showHBar = showHBar_
    , labelAlign = labelAlign_
    , tooltips = cfg.tooltips
    , baseFontSize = cfg.baseFontSize
    , style = genStyle cfg.baseFontSize cfg.chartSpec cfg.fontSpec cfg.tooltips
    , legend = cfg.legend
    }


getLabels : List (Cfg.GridSeries Cfg.GridPair) -> ( List String, List String )
getLabels data =
    let
        xs =
            Utils.snds data
                |> List.head
                |> Maybe.withDefault []
                |> Utils.fsts

        ys =
            Utils.fsts data
    in
    ( "_" :: xs, "labels" :: ys )


genDataScales :
    List (Cfg.GridSeries Cfg.GridPair)
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


parseChartSpec : Cfg.ChartSpec -> ( Bool, Cfg.Position )
parseChartSpec spec =
    case spec of
        Cfg.FacetGridChartSpec s ->
            ( s.showHBar, s.labelAlign )

        _ ->
            ( False, Cfg.Inline )


getDims :
    BandScale String
    -> BandScale String
    -> Maybe (Cfg.GridSeries Cfg.GridPair)
    -> Bool
    -> ( Float, Float )
getDims xScale yScale series showHBar =
    let
        pairs =
            case series of
                Just ( name, facets ) ->
                    List.head facets
                        |> Maybe.withDefault ( "", [] )
                        |> Utils.snd

                Nothing ->
                    []

        cellW =
            Scale.bandwidth xScale
                / (if showHBar then
                    2.0

                   else
                    1.0
                  )
                / (List.length pairs |> toFloat)
                |> min (Scale.bandwidth yScale)

        cellH =
            cellW
    in
    ( cellW, cellH )



--------------------------------------------------------------------------------
-- Render


render : Cfg.GridChartCfg -> List (Cfg.GridSeries Cfg.GridPair) -> Svg msg
render cfg data =
    let
        env =
            genChartEnv cfg data

        facet_labels =
            List.head data
                |> Maybe.withDefault ( "", [] )
                |> Utils.snd
                |> Utils.fsts
    in
    svg
        [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g
            [ class [ "x_labels" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (renderXLabels env <| Utils.fsts data)
        , g
            [ class [ "y_labels" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (List.map (renderYLabel env) facet_labels)
        , g
            [ class [ "grid_data" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (List.map (renderGrid env) data)
        , g
            [ class [ "grid_tooltips" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (List.map (renderTooltips env) data)
        ]


sortByRecent : List (Cfg.GridSeries Cfg.GridPair) -> List (Cfg.GridSeries Cfg.GridPair)
sortByRecent =
    List.sortBy
        (Utils.snd
            >> List.head
            >> Maybe.withDefault ( "", [] )
            >> Utils.snd
            >> Utils.snds
            >> LE.last
            >> Maybe.withDefault 0
        )
        >> List.reverse


renderXLabels : ChartEnv -> List String -> List (Svg msg)
renderXLabels env xs =
    let
        xs_ =
            case env.labelAlign of
                Cfg.Right ->
                    Utils.alignRight xs

                _ ->
                    xs
    in
    List.map2 (renderXLabel env) xs xs_


renderXLabel : ChartEnv -> String -> String -> Svg msg
renderXLabel env lbl lblDisplay =
    text_
        [ x <| Scale.convert env.xScale "labels"
        , y <| Scale.convert env.yScale lbl + toFloat env.baseFontSize
        ]
        [ text lblDisplay ]


renderYLabel : ChartEnv -> String -> Svg msg
renderYLabel env lbl =
    let
        x_ =
            Scale.convert env.yScale "labels"

        nameX =
            String.length lbl
                * env.baseFontSize
                |> toFloat
                |> (*) (0.7 / 2)
                |> (-) (x_ + Scale.bandwidth env.xScale / 2)
    in
    text_
        [ x <| Scale.convert env.xScale lbl + nameX
        , y <| Scale.convert env.yScale "labels" + toFloat env.baseFontSize
        ]
        [ text lbl ]


renderGrid : ChartEnv -> Cfg.GridSeries Cfg.GridPair -> Svg msg
renderGrid env ( lbl, facets ) =
    let
        y =
            Scale.convert env.yScale lbl
    in
    g [ class [ "grid_facet" ] ]
        (List.map (renderCells env y) facets
            ++ (if env.showHBar then
                    List.map2 (renderHBars env y) facets env.dataScales

                else
                    []
               )
        )


renderCells :
    ChartEnv
    -> Float
    -> ( String, List Cfg.GridPair )
    -> Svg msg
renderCells env y_ ( name, pairs ) =
    let
        x_ =
            Scale.convert env.xScale name

        w =
            Scale.bandwidth env.xScale

        x0 =
            if env.showHBar then
                x_

            else
                (List.length pairs |> toFloat)
                    * env.cellW
                    / 2
                    |> (-) (x_ + w / 2)

        f colorVal ( xStart, acc ) =
            ( xStart + env.cellW
            , renderCell xStart y_ env.cellW colorVal :: acc
            )
    in
    g
        []
        (relativeScale pairs
            |> List.foldl f ( x0, [] )
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



-- Scale list tail as percentage of list head


relativeScale : List ( a, Float ) -> List Float
relativeScale xs =
    let
        f val ( scalar, acc ) =
            max -1.0 (min 1.0 ((val - scalar) / scalar))
                |> (\val_ -> ( scalar, val_ :: acc ))
    in
    case Utils.snds xs of
        [] ->
            []

        y :: ys ->
            List.foldl f ( y, [ 0 ] ) ys |> Utils.snd |> List.reverse


renderHBars :
    ChartEnv
    -> Float
    -> ( String, List Cfg.GridPair )
    -> ContinuousScale Float
    -> Svg msg
renderHBars env y_ ( name, pairs ) dScale =
    let
        x_ =
            Scale.convert env.xScale name
                |> (+) (Scale.bandwidth env.xScale / 2)
                |> (+) 5

        last =
            LE.last pairs |> Maybe.withDefault ( "", 0 )
    in
    g [ class [ "grid_hbar" ] ]
        [ rect
            [ x x_
            , y <| y_ + env.cellH * 0.2
            , width <| Scale.convert dScale (Utils.snd last)
            , height <| env.cellH * 0.6
            ]
            []
        ]



--------------------------------------------------------------------------------
-- Render Tooltips


type alias HasTooltipEnv a =
    { a
        | w : Float
        , h : Float
        , pad : Cfg.Padding
        , tooltips : Cfg.Tooltips
        , xScale : BandScale String
        , yScale : BandScale String
    }


type alias HoverEnv =
    { x : Float
    , y : Float
    , h : Float
    , w : Float
    }


renderTooltips : ChartEnv -> Cfg.GridSeries Cfg.GridPair -> Svg msg
renderTooltips env ( lbl, facets ) =
    g [ class [ "grid_facet" ] ]
        (if env.tooltips.showHoverTooltips then
            List.map (renderHoverTooltips env lbl) facets

         else
            []
        )


renderHoverTooltips :
    ChartEnv
    -> String
    -> ( String, List Cfg.GridPair )
    -> Svg msg
renderHoverTooltips env lbl ( name, pairs ) =
    g [ class [ "grid_tooltip_area" ] ]
        [ rect
            [ class [ "transparent" ]
            , x <| Scale.convert env.xScale name
            , y <| Scale.convert env.yScale lbl
            , width <| Scale.bandwidth env.xScale
            , height <| Scale.bandwidth env.yScale
            ]
            []
        , renderHoverTooltip env lbl name pairs
        ]


renderHoverTooltip :
    HasTooltipEnv a
    -> String
    -> String
    -> List ( String, Float )
    -> Svg msg
renderHoverTooltip env lbl name points =
    let
        hEnv =
            genHoverEnv env lbl name points

        pad =
            5

        ( lines, lineParams ) =
            genHoverText env points ( hEnv.x, hEnv.y )
    in
    g [ class [ "grid_tooltip_hover" ] ]
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
            [ text <| lbl ++ ": " ++ name ]
         ]
            ++ List.map2 (renderHoverText pad) lines lineParams
        )


renderHoverText : Float -> String -> ( Float, Float ) -> Svg msg
renderHoverText pad txt ( hx, hy ) =
    text_
        [ x <| hx + pad
        , y <| hy + pad
        ]
        [ text txt ]


genHoverEnv :
    HasTooltipEnv a
    -> String
    -> String
    -> List ( String, Float )
    -> HoverEnv
genHoverEnv env lbl name pairs =
    let
        ( xOffset, yOffset ) =
            ( 5, 5 )

        sz =
            env.tooltips.hoverTooltipSize |> toFloat

        hh =
            (List.length pairs + 2 |> toFloat) * sz * 1.03

        hw =
            max (String.length lbl) (Utils.pairWidthMax pairs 2)
                |> toFloat
                |> (\n -> n * (sz * 0.65))

        xw =
            Scale.bandwidth env.xScale

        hx =
            Scale.convert env.xScale name

        hx_ =
            if hx + hw + xw + xOffset > (env.w - env.pad.left - env.pad.right) then
                hx - hw - xOffset

            else
                hx + xw + xOffset

        hy =
            Scale.convert env.yScale lbl

        hy_ =
            if hy + hh + yOffset > (env.h - env.pad.bottom - env.pad.top) then
                hy - hh - yOffset

            else
                hy + yOffset
    in
    { x = hx_
    , y = hy_
    , w = hw
    , h = hh
    }


genHoverText :
    HasTooltipEnv a
    -> List ( String, Float )
    -> ( Float, Float )
    -> ( List String, List ( Float, Float ) )
genHoverText env pairs ( x0, y0 ) =
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

        xs =
            List.repeat (List.length pairs) x0

        ys =
            List.range 2 (List.length pairs + 1)
                |> List.map
                    (\i -> y0 + (i * env.tooltips.hoverTooltipSize |> toFloat) + 5)
    in
    ( pairs |> List.map fmt
    , List.map2 Tuple.pair xs ys
    )



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
    Scale.band cfg ( 0, h - padding ) ys


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


genStyle : Int -> Cfg.ChartSpec -> Cfg.FontSpec -> Cfg.Tooltips -> String
genStyle sz cCfg fCfg tCfg =
    let
        fillColor =
            case cCfg of
                Cfg.FacetGridChartSpec spec ->
                    spec.barFillColor

                _ ->
                    defaultFillColor
    in
    genBaseStyle sz fillColor fCfg tCfg


genBaseStyle : Int -> String -> Cfg.FontSpec -> Cfg.Tooltips -> String
genBaseStyle sz fillColor fCfg tCfg =
    """
     text { font-family: {{typeface}}, monospace, sans-serif;
            fill: {{textColor}}; }
     .x_labels, .y_labels { font-size: {{sz}}px; }
     .grid_hbar rect { fill: {{fillColor}}; }
     .transparent { opacity: 0.0; }
     .grid_tooltip_hover { display: none; font-size: {{szH}}px;}
     .grid_tooltip_hover rect { fill: rgba(250, 250, 250, 1.0); }
     .grid_tooltip_area:hover .grid_tooltip_hover { display: inline; }
     """
        |> String.Format.namedValue "sz" (String.fromInt sz)
        |> String.Format.namedValue "textColor" fCfg.textColor
        |> String.Format.namedValue "szH" (String.fromInt tCfg.hoverTooltipSize)
        |> String.Format.namedValue "typeface" fCfg.typeface
        |> String.Format.namedValue "fillColor" fillColor
        |> String.Format.namedValue "showHover"
            (UI.display tCfg.showHoverTooltips)


defaultFillColor : String
defaultFillColor =
    Defaults.rgbaToString Defaults.defaultFillColor
