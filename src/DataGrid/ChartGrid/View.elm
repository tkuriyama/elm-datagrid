module DataGrid.ChartGrid.View exposing ( view )

{-| Render module for ChartGrid

-}

import Element exposing ( Element, alignLeft, alignRight, centerX, centerY
                        , column, el, fill, height, paragraph , padding, px
                        , row, shrink, spacing, text, width )
import Element.Font as Font
import Element.Input as Input
import Html exposing ( Html )

import DataGrid.Config as Cfg exposing ( ChartCfg (..), ChartData (..)
                                       , ChartSpec (..) )
import DataGrid.ChartGrid.Types exposing (..)
import DataGrid.Internal.Generic as Generic
import DataGrid.Internal.UI as UI
import DataGrid.Internal.Utils as Utils
import DataGrid.LineChart as LC


--------------------------------------------------------------------------------

type alias Attributes msg
    = List (Element.Attribute msg)

--------------------------------------------------------------------------------
-- View

view : Model label -> Html Msg
view model =
    let cfg = model.cfg
        xss = model.charts
        w = Maybe.withDefault 1800 cfg.w
        rows = List.map genRow xss
        genRow xs = row [ width fill, spacing cfg.colSpacing ]
                        (List.map (chartCell cfg) xs)
        gridTitle = title cfg cfg.textColor cfg.gridBaseFontSize
    in
        Element.layout
            [ Font.family [ Font.typeface cfg.typeface, Font.sansSerif ]
            , padding cfg.padding
            ]
            ( column
                 [ centerX, width <| px w, spacing cfg.rowSpacing ]
                 ( [gridTitle, text "\n"] ++ rows )
            )

chartCell : LayoutCfg -> ChartCell label -> Element Msg
chartCell cfg cell =
    let chart = project cell |> Generic.render cell.chartCfg
    in column
        [ width fill ]
        [ title cell cfg.textColor cfg.cellBaseFontSize
        , controls cell
        , chart |> Element.html
        ]

project : ChartCell label -> ChartData label
project cell =
    case cell.chartData of
        LineChartData d ->
            d
             |> (if cell.showRelative then LC.projectRelative else identity)
             |> (if cell.showFirstDeriv then LC.projectFirstDeriv else identity)
             |> LC.projectSeries cell.hideSeries
             |> LineChartData
        _ ->
            cell.chartData

title : HasTitleDesc a -> Element.Color -> Int -> Element Msg
title r textColor baseFont =
    let t = Maybe.withDefault "" r.title
        d = Maybe.withDefault "" r.description
        smallFont = round <| toFloat baseFont * 0.8
    in paragraph
        [ Font.color textColor ]
        [ el [ Font.bold, Font.size baseFont ] (text t)
        , text " | "
        , el [ Font.size smallFont ] (text d)
        ]

controls : ChartCell label -> Element Msg
controls cell =
    let ((_, _, _), toggleH) = parseToggles cell.chartCfg
        attrs = [ width shrink
                , Font.size <| round (toFloat toggleH * 0.7)
                ]
    in row
        [ width fill, spacing 2 ]
        [ controlSeries attrs cell, controlRelFD attrs cell ]

controlSeries : Attributes Msg -> ChartCell label -> Element Msg
controlSeries attrs cell =
    let attrs_ = attrs ++ [ alignLeft, spacing 5 ]
        ((toggleSs, _, _), _) = parseToggles cell.chartCfg
        f name = Input.checkbox attrs
                 { onChange = ToggleSeries cell.index name
                 , icon = Input.defaultCheckbox
                 , checked = List.member name cell.hideSeries |> not
                 , label = Input.labelRight [centerY] (text name)
                 }
    in if not toggleSs then Element.none
       else case cell.chartData of
           LineChartData d ->
               row attrs_ (Utils.fsts d |> List.map f)
           _ ->
               Element.none

controlRelFD : List (Element.Attribute Msg) ->
               ChartCell label ->
               Element Msg
controlRelFD attrs cell =
    let attrs_ = attrs ++ [ alignRight, UI.padRight 5 ]
        ((_, toggleRel, toggleFD), toggleH) = parseToggles cell.chartCfg
    in row
        attrs_
        [ if not toggleRel then Element.none
          else
              Input.checkbox attrs
              { onChange = ToggleRelative cell.index
              , icon = UI.toggle toggleH "Relative" "Notional"
              , checked = cell.showRelative
              , label = Input.labelHidden "Notional / Relative"
              }
        , if not toggleFD then Element.none
          else
              Input.checkbox attrs
                  { onChange = ToggleFirstDeriv cell.index
                  , icon = UI.toggle toggleH "1Δ " " 0Δ"
                  , checked = cell.showFirstDeriv
                  , label = Input.labelHidden "0th Dtj Deriv / 1st Deriv"
                  }
        ]

parseToggles : Cfg.ChartCfg label -> ((Bool, Bool, Bool), Int)
parseToggles cfg =
    let noToggle = ((False, False, False), 0)
    in case cfg of
        Std c -> case c.chartSpec of
                     LineChartSpec spec -> ( ( spec.toggleSeries
                                             , spec.toggleRelative
                                             , spec.toggleFirstDeriv
                                             )
                                           , spec.toggleHeight
                                           )
                     _ -> noToggle
        DefaultChartCfg -> noToggle

