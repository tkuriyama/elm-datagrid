module DataGrid.TreeGridChart exposing (..)

{-| Render a a single Tree Grid Chart.
-}

import Axis
import Color exposing (Color)
import DataGrid.ChartConfig as Cfg
import DataGrid.Internal.Defaults as Defaults
import DataGrid.Internal.GridChart as GridChart
import DataGrid.Internal.SquarifiedTreemap as ST
import DataGrid.Internal.UI as UI
import DataGrid.Internal.Utils as Utils
import List.Extra as LE
import List.Nonempty as NE
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
        ( fontSize
        , height
        , rx
        , strokeWidth
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
    , pad : Cfg.Padding
    , tooltips : Cfg.Tooltips
    , innerPad : Int
    , baseFontSize : Int
    , minFontSize : Int
    , style : String
    }


genChartEnv : Cfg.GridChartCfg -> List (Cfg.GridSeries Cfg.GridTriple) -> ChartEnv
genChartEnv cfg data =
    let
        ( innerPad_, baseFontSize_, minFontSize_ ) =
            parseChartSpec cfg.chartSpec
    in
    { w = cfg.w
    , h = cfg.h
    , pad = cfg.pad
    , tooltips = cfg.tooltips
    , innerPad = innerPad_
    , baseFontSize = baseFontSize_
    , minFontSize = minFontSize_
    , style = genStyle cfg baseFontSize_
    }


parseChartSpec : Cfg.ChartSpec -> ( Int, Int, Int )
parseChartSpec spec =
    case spec of
        Cfg.TreeGridChartSpec s ->
            ( s.innerPad, s.cellBaseFontSize, s.cellMinFontSize )

        _ ->
            ( 0, 0, 0 )



--------------------------------------------------------------------------------
-- Render


render : Cfg.GridChartCfg -> List (Cfg.GridSeries Cfg.GridTriple) -> Svg msg
render cfg data =
    let
        env =
            genChartEnv cfg data
    in
    case data of
        [] ->
            svg [] []

        z :: zs ->
            renderNonEmpty env (NE.Nonempty z zs)


renderNonEmpty : ChartEnv -> NE.Nonempty (Cfg.GridSeries Cfg.GridTriple) -> Svg msg
renderNonEmpty env data =
    let
        dims =
            { x = env.w - env.pad.left - env.pad.right
            , y = env.h - env.pad.top - env.pad.bottom
            }

        groups =
            genGroups (dims.x * dims.y) data

        groupCells =
            ST.makeTreemap dims groups

        subtrees =
            NE.map2 genSubtree groups groupCells
    in
    svg [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        , g
            [ class [ "tree_cell" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (NE.map (renderSubtree env) subtrees |> NE.toList)
        , g
            [ class [ "tree_group" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (NE.map (renderGroupCell env) groupCells |> NE.toList)
        , g
            [ class [ "tree_hover" ]
            , transform [ Translate env.pad.left env.pad.top ]
            ]
            (NE.map (renderTreeHover env) subtrees |> NE.toList)
        ]



--------------------------------------------------------------------------------
-- Groups


type alias Group =
    { name : String
    , area : Float
    , series : NE.Nonempty TreeTriple
    }


type alias TreeTriple =
    ( String, Cfg.GridTriple, Cfg.GridTriple )


renderGroupCell : ChartEnv -> ST.Cell -> Svg msg
renderGroupCell env cell =
    rect
        [ x cell.x
        , y cell.y
        , width cell.w
        , height cell.h
        ]
        []


genGroups : Float -> NE.Nonempty (Cfg.GridSeries Cfg.GridTriple) -> NE.Nonempty Group
genGroups area data =
    let
        groups =
            NE.map genGroup data

        totalWeight =
            NE.map .area groups |> NE.foldl1 (+)

        scalar =
            area / totalWeight
    in
    NE.sortBy (.area >> (*) -1) groups
        |> NE.map (\s -> { s | area = s.area * scalar })


genGroup : Cfg.GridSeries Cfg.GridTriple -> Group
genGroup ( groupName, pairs ) =
    let
        treeSeries =
            genTreeSeries pairs

        area =
            sumWeights treeSeries
    in
    { name = groupName
    , area = area
    , series = treeSeries
    }


genTreeSeries : List ( String, List Cfg.GridTriple ) -> NE.Nonempty TreeTriple
genTreeSeries pairs =
    let
        default =
            ( "", 0, 0 )

        f ( name, triples ) =
            case triples of
                x :: y :: [] ->
                    ( name, x, y )

                _ ->
                    ( name, default, default )
    in
    case pairs of
        [] ->
            NE.fromElement ( "", default, default )

        x :: xs ->
            NE.Nonempty (f x) (List.map f xs)
                |> NE.sortBy (\( _, _, ( _, w, _ ) ) -> w * -1)


sumWeights : NE.Nonempty TreeTriple -> Float
sumWeights =
    NE.map (\( _, _, ( _, w, _ ) ) -> w) >> NE.foldl1 (+)



--------------------------------------------------------------------------------
-- Subtrees


type alias Subtree =
    ( ST.Cell, NE.Nonempty TreeCell, ST.SquarifiedTreemap )


type alias TreeCell =
    { groupName : String
    , cellName : String
    , previousLabel : String
    , previousValue : Float
    , currentLabel : String
    , currentValue : Float
    , area : Float
    }


genSubtree : Group -> ST.Cell -> Subtree
genSubtree group groupCell =
    let
        dims =
            { x = groupCell.w, y = groupCell.h }

        areaScalar =
            group.area / sumWeights group.series

        treeCells =
            NE.map (genTreeCell group.name areaScalar) group.series

        treemap =
            ST.makeTreemap dims treeCells

    in
    ( groupCell, treeCells, treemap )


genTreeCell : String -> Float -> TreeTriple -> TreeCell
genTreeCell groupName areaScalar triple =
    let
        ( cellName, ( pLbl, pWeight, pVal ), ( cLbl, cWeight, cVal ) ) =
            triple
    in
    { groupName = groupName
    , cellName = cellName
    , previousLabel = pLbl
    , previousValue = pVal
    , currentLabel = cLbl
    , currentValue = cVal
    , area = cWeight * areaScalar
    }


renderSubtree : ChartEnv -> Subtree -> Svg msg
renderSubtree env ( groupCell, treeCells, treemap ) =
    g
        [ transform [ Translate groupCell.x groupCell.y ]
        ]
        (NE.map2 (renderTreeCell env) treeCells treemap
            |> NE.toList
        )


renderTreeCell : ChartEnv -> TreeCell -> ST.Cell -> Svg msg
renderTreeCell env t cell =
    let
        length =
            String.length t.cellName * env.minFontSize |> toFloat |> (*) 0.7

        fontSz =
            min
                (toFloat env.baseFontSize)
                (cell.w / 0.7 / (String.length t.cellName |> toFloat))

        fits =
            length * 1.2 < cell.w && fontSz * 1.2 < cell.h

        colorVal =
            (t.currentValue - t.previousValue) / t.previousValue
    in
    g []
        ([ rect
            [ x cell.x
            , y cell.y
            , width cell.w
            , height cell.h
            , rx 1
            , fill <| Paint <| GridChart.getColor (colorVal * 10)
            ]
            []
         ]
            ++ (if fits then
                    [ text_
                        [ x <| cell.x + toFloat env.innerPad
                        , y <| cell.y + toFloat env.innerPad + fontSz
                        , fontSize fontSz
                        ]
                        [ text t.cellName ]
                    ]

                else
                    []
               )
        )



--------------------------------------------------------------------------------
-- Hover Tooltips


type alias HasTooltipEnv a =
    { a
        | w : Float
        , h : Float
        , pad : Cfg.Padding
        , tooltips : Cfg.Tooltips
    }


renderTreeHover : ChartEnv -> Subtree -> Svg msg
renderTreeHover env ( groupCell, treeCells, treemap ) =
    if env.tooltips.showHoverTooltips then
        g [ transform [ Translate groupCell.x groupCell.y ] ]
            (NE.map2 (renderTreeCellHover env groupCell) treeCells treemap
                |> NE.toList
            )

    else
        g [] []


renderTreeCellHover : ChartEnv -> ST.Cell -> TreeCell -> ST.Cell -> Svg msg
renderTreeCellHover env groupCell t cell =
    g [ class [ "tree_tooltip_area" ] ]
        [ rect
            [ class [ "transparent" ]
            , x <| cell.x
            , y <| cell.y
            , width <| cell.w
            , height <| cell.h
            ]
            []
        , renderHoverTooltip env groupCell t cell
        ]


renderHoverTooltip :
    HasTooltipEnv a
    -> ST.Cell
    -> TreeCell
    -> ST.Cell
    -> Svg msg
renderHoverTooltip env groupCell t cell =
    let
        lines =
            genHoverLines t

        hEnv =
            genHoverEnv env lines groupCell cell

        lineCoords =
            GridChart.genHoverLineCoords env lines ( hEnv.x, hEnv.y )

        pad =
            5
    in
    g [ class [ "tree_tooltip_hover" ] ]
        ([ rect
            [ class [ "invert" ]
            , x hEnv.x
            , y hEnv.y
            , height hEnv.h
            , width hEnv.w
            , rx 3
            ]
            []
         ]
            ++ (NE.map2 (GridChart.renderHoverText pad) lines lineCoords
                    |> NE.toList
               )
        )


genHoverEnv :
    HasTooltipEnv a
    -> NE.Nonempty String
    -> ST.Cell
    -> ST.Cell
    -> GridChart.HoverEnv
genHoverEnv env lines groupCell cell =
    let
        ( xOffset, yOffset ) =
            ( 5, 5 )

        sz =
            env.tooltips.hoverTooltipSize |> toFloat

        ( hw, hh ) =
            GridChart.hoverDims sz lines

        hx =
            if (groupCell.x + cell.x + cell.w / 2) > env.w / 2 then
                cell.x + cell.w / 2 - xOffset - hw

            else
                cell.x + cell.w / 2 + xOffset

        hy =
            if (groupCell.y + cell.y + cell.h / 2) > env.h / 2 then
                cell.y + cell.h / 2 - yOffset - hh

            else
                cell.y + cell.h / 2 + yOffset
    in
    { x = hx
    , y = hy
    , w = hw
    , h = hh
    }


genHoverLines : TreeCell -> NE.Nonempty String
genHoverLines t =
    NE.Nonempty
        ( t.groupName, t.cellName )
        [ ( t.previousLabel, Utils.fmtFloat 2 t.previousValue )
        , ( t.currentLabel, Utils.fmtFloat 2 t.currentValue )
        ]
        |> GridChart.pairsToStrings identity 3



--------------------------------------------------------------------------------
-- Style


genStyle : Cfg.GridChartCfg -> Int -> String
genStyle cfg sz =
    let
        ( fCfg, tCfg ) =
            ( cfg.fontSpec, cfg.tooltips )
    in
    """
     text { font-family: {{typeface}}, monospace, sans-serif;
            fill: {{textColor}}; }
     .tree_group rect { display: inline; fill: none;
                        stroke: rgb(120, 120, 120); stroke-width: 1.5px; }
     .tree_cell rect { display: inline;
                       stroke: rgb(160, 160, 160); stroke-width: 0.5px; }
     .tree_cell text { opacity: 0.85; }
     .transparent { opacity : 0.0; }
     .tree_tooltip_hover { display: none; font-size: {{szH}}px;}
     .tree_tooltip_hover rect { fill: rgba(250, 250, 250, 1.0); }
     .tree_tooltip_area:hover .tree_tooltip_hover { display: inline; }
     """
        |> String.Format.namedValue "sz" (String.fromInt sz)
        |> String.Format.namedValue "textColor" fCfg.textColor
        |> String.Format.namedValue "szH" (String.fromInt tCfg.hoverTooltipSize)
        |> String.Format.namedValue "typeface" fCfg.typeface
        |> String.Format.namedValue "showHover"
            (UI.display tCfg.showHoverTooltips)


