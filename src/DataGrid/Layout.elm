module DataGrid.Layout exposing (Chart, LayoutConfig, chartGrid)

{-| Create grids using elm-ui.

A grid is just an n x m matric of elements (typically of the same type),
where an element is an elm-ui `Element msg`.

-}

import Element exposing (Element, el, text, row, alignRight, fill, width, px,
                             paragraph,
                         rgb255, spacing, centerY, padding, wrappedRow, column)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)

import Internal.Defaults as Defaults


--------------------------------------------------------------------------------

type alias LayoutConfig =
    { w : Int
    , colSpacing : Int
    , rowSpacing : Int
    , padding : Int
    , title : Maybe String
    , description : Maybe String
    , fontColor : Maybe Element.Color
    , fontFamily : Maybe Font.Font
    , gridBaseFontSize : Int
    , cellBaseFontSize : Int
    }

type alias Chart msg =
    { title : Maybe String
    , description : Maybe String
    , chart : Element msg
    }

type alias HasTitleDesc a =
    { a | title : Maybe String, description : Maybe String }


--------------------------------------------------------------------------------
-- Chart Grid

chartGrid : LayoutConfig -> List (List (Chart msg)) -> Html msg
chartGrid cfg xss =
    let rows = List.map genRow xss
        genRow xs = row [ width fill, spacing cfg.colSpacing ]
                        (List.map (chartCell cfg) xs)
        gridTitle = title cfg cfg.fontColor cfg.gridBaseFontSize
    in
        Element.layout
            []
            (column
                 [ width <| px cfg.w, spacing cfg.rowSpacing ]
                 ([gridTitle, text "\n"] ++ rows))

chartCell : LayoutConfig -> Chart msg -> Element msg
chartCell cfg c =
    let t = Maybe.withDefault "" c.title
        d = Maybe.withDefault "" c.description
    in column
        [ width fill ]
        [ title c cfg.fontColor cfg.cellBaseFontSize
        , c.chart ]


--------------------------------------------------------------------------------
-- Helpers

title : HasTitleDesc a -> Maybe Element.Color -> Int -> Element msg
title r mColor baseFont =
    let t = Maybe.withDefault "" r.title
        d = Maybe.withDefault "" r.description
        fc = Maybe.withDefault (Defaults.rgbToElmUI Defaults.defaultTextColor)
             mColor
        smallFont = round <| (toFloat baseFont) * 0.8
    in paragraph
        [ Font.color fc ]
        [ el [ Font.bold, Font.size baseFont ] (text t)
        , text " | "
        , el [ Font.size smallFont ] (text d)
        ]
