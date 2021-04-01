module DataGrid.ChartGrid.View exposing (view)

{-| Render module for ChartGrid.
-}

import DataGrid.ChartGrid.Types as T exposing (..)
import DataGrid.Config as Cfg
    exposing
        ( ChartCfg(..)
        , ChartData(..)
        , ChartSpec(..)
        )
import DataGrid.Internal.Generic as Generic
import DataGrid.Internal.StdChart as StdChart
import DataGrid.Internal.UI as UI
import DataGrid.Internal.Utils as Utils
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



--------------------------------------------------------------------------------


type alias Attributes msg =
    List (Element.Attribute msg)



--------------------------------------------------------------------------------
-- View


view : Model label -> Html Msg
view model =
    let
        cfg =
            model.cfg

        ( w, h ) =
            ( UI.maybeLength cfg.w fill, UI.maybeLength cfg.h fill )

        gridTitle =
            title cfg cfg.textColor cfg.gridBaseFontSize
    in
    Element.layout
        [ Font.family [ Font.typeface cfg.typeface, Font.sansSerif ]
        , padding cfg.padding
        ]
        (column
            [ alignTop, centerX, height h, width w, spacing cfg.rowSpacing ]
            [ el [ UI.padBottom 10, width fill ] gridTitle
            , parseGrid model.cfg model.charts
            ]
        )


parseGrid : LayoutCfg -> ChartGrid label -> Element Msg
parseGrid cfg grid =
    case grid of
        T.Column ( mw, mh ) cells ->
            let
                ( w, h ) =
                    ( UI.maybeLength mw fill, UI.maybeLength mh fill )
            in
            column
                [ alignTop, width w, height h, spacing cfg.rowSpacing ]
                (List.map (parseGrid cfg) cells)

        T.Row ( mw, mh ) cells ->
            let
                ( w, h ) =
                    ( UI.maybeLength mw fill, UI.maybeLength mh fill )
            in
            row
                [ alignTop, width w, height h, spacing cfg.colSpacing ]
                (List.map (parseGrid cfg) cells)

        T.TabbedCell name cells ->
            tabbedChartCell name cfg cells

        T.Cell cell ->
            chartCell cfg cell


chartCell : LayoutCfg -> ChartCell label -> Element Msg
chartCell cfg cell =
    let
        chart =
            project cell |> Generic.render cell.chartCfg
    in
    column
        [ alignTop, width fill ]
        [ title cell cfg.textColor cfg.cellBaseFontSize
        , controls cell
        , chart |> Element.html
        ]


tabbedChartCell :
    String
    -> LayoutCfg
    -> List ( String, ChartCell label )
    -> Element Msg
tabbedChartCell active cfg cells =
    let
        f ( name, cell ) acc =
            if active == name then
                chartCell cfg cell

            else
                acc
    in
    column
        [ alignTop, width fill, spacing 5 ]
        [ tabs active cfg cells
        , List.foldr f Element.none cells
        ]


tabs :
    String
    -> LayoutCfg
    -> List ( String, ChartCell label )
    -> Element Msg
tabs active cfg cells =
    row
        [ Font.size <| round <| toFloat cfg.cellBaseFontSize
        , centerX
        , alignLeft
        ]
        (List.map (tab active) <| Utils.fsts cells)


tab : String -> String -> Element Msg
tab active name =
    let
        borders =
            { bottom = 0, top = 1, left = 1, right = 1 }

        corners =
            { topLeft = 6, topRight = 6, bottomLeft = 0, bottomRight = 0 }

        background =
            if active == name then
                Element.rgb 0.8 0.8 0.8

            else
                Element.rgb 1 1 1
    in
    Input.button
        [ paddingXY 7 5
        , Border.widthEach borders
        , Border.roundEach corners
        , Border.color <| Element.rgb 0.3 0.3 0.3
        , Background.color background
        ]
        { onPress = Just <| ActivateTab name
        , label = text name
        }


title : HasTitleDesc a -> Element.Color -> Int -> Element Msg
title r textColor baseFont =
    let
        t =
            Maybe.withDefault "" r.title

        d =
            Maybe.withDefault "" r.description

        smallFont =
            round <| toFloat baseFont * 0.8
    in
    paragraph
        [ Font.color textColor, width fill ]
        [ el [ Font.bold, Font.size baseFont ] (text t)
        , text " | "
        , el [ Font.size smallFont ] <| text d
        , el [ Font.size smallFont, alignRight, UI.padRight 30 ] <|
            UI.genLinks r.links
        ]



--------------------------------------------------------------------------------
-- Data Projection


project : ChartCell label -> ChartData label
project cell =
    case cell.chartData of
        BarChartStackedData d ->
            d
                |> StdChart.projectSeries cell.hideSeries
                |> (if cell.showRelative then
                        StdChart.projectRelative

                    else
                        identity
                   )
                |> BarChartStackedData

        LineChartData d ->
            d
                |> StdChart.projectSeries cell.hideSeries
                |> (if cell.showRelative then
                        StdChart.projectRelative

                    else
                        identity
                   )
                |> (if cell.showFirstDeriv then
                        StdChart.projectFirstDeriv

                    else
                        identity
                   )
                |> LineChartData

        _ ->
            cell.chartData



--------------------------------------------------------------------------------
-- Control Components


controls : ChartCell label -> Element Msg
controls cell =
    let
        ( ( _, _, _ ), toggleH ) =
            parseToggles cell.chartCfg

        attrs =
            [ width shrink
            , Font.size <| round (toFloat toggleH * 0.7)
            ]
    in
    row
        [ width fill, spacing 2 ]
        [ controlSeries attrs cell
        , text " "
        , controlRelFD attrs cell
        ]


controlSeries : Attributes Msg -> ChartCell label -> Element Msg
controlSeries attrs cell =
    let
        attrs_ =
            attrs ++ [ alignLeft, spacing 10 ]

        ( ( toggleSs, _, _ ), _ ) =
            parseToggles cell.chartCfg

        f name =
            Input.checkbox attrs
                { onChange = ToggleSeries cell.index name
                , icon = Input.defaultCheckbox
                , checked = List.member name cell.hideSeries |> not
                , label = Input.labelRight [ centerY ] (text name)
                }
    in
    if not toggleSs then
        Element.none

    else
        case cell.chartData of
            BarChartStackedData d ->
                row attrs_ (Utils.fsts d |> List.map f)

            LineChartData d ->
                row attrs_ (Utils.fsts d |> List.map f)

            _ ->
                Element.none


controlRelFD :
    List (Element.Attribute Msg)
    -> ChartCell label
    -> Element Msg
controlRelFD attrs cell =
    let
        attrs_ =
            attrs ++ [ alignRight, UI.padRight 30 ]

        ( ( _, toggleRel, toggleFD ), toggleH ) =
            parseToggles cell.chartCfg
    in
    row
        attrs_
        [ if not toggleRel then
            Element.none

          else
            Input.checkbox attrs
                { onChange = ToggleRelative cell.index
                , icon = UI.toggle toggleH "Relative" "Nominal"
                , checked = cell.showRelative
                , label = Input.labelHidden "Nominal / Relative"
                }
        , if not toggleFD then
            Element.none

          else
            Input.checkbox attrs
                { onChange = ToggleFirstDeriv cell.index
                , icon = UI.toggle toggleH "1Δ " " 0Δ"
                , checked = cell.showFirstDeriv
                , label = Input.labelHidden "0th Dtj Deriv / 1st Deriv"
                }
        ]


parseToggles : Cfg.ChartCfg label -> ( ( Bool, Bool, Bool ), Int )
parseToggles cfg =
    let
        noToggle =
            ( ( False, False, False ), 0 )
    in
    case cfg of
        Std c ->
            case c.chartSpec of
                BarChartStackedSpec spec ->
                    ( ( spec.toggleSeries
                      , spec.toggleRelative
                      , False
                      )
                    , spec.toggleHeight
                    )

                LineChartSpec spec ->
                    ( ( spec.toggleSeries
                      , spec.toggleRelative
                      , spec.toggleFirstDeriv
                      )
                    , spec.toggleHeight
                    )

                _ ->
                    noToggle

        Grid c ->
            noToggle

        DefaultChartCfg ->
            noToggle
