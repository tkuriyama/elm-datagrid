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


--------------------------------------------------------------------------------


type alias LayoutConfig =
    { w : Int
    , spacing : Int
    , title : Maybe String
    , description : Maybe String
    , gridBaseFontSize : Int
    , cellBaseFontSize : Int
    }

type alias Chart msg =
    { title : Maybe String
    , description : Maybe String
    , chart : Element msg
    }


--------------------------------------------------------------------------------
-- Chart Grid

chartGrid : Float -> Float -> List (Chart msg) -> Html msg
chartGrid w h charts =
    let r = row [ width fill, spacing 10 ] (List.map chartCell charts)
    in
    Element.layout
        []
        (column [ width <| px 1800, spacing 10 ] [r, r, r])

chartCell : Chart msg -> Element msg
chartCell c =
    let t = Maybe.withDefault "" c.title
        d = Maybe.withDefault "" c.description
    in column
        [ padding 0 , width fill ]
        [ paragraph
              [ Font.color (rgb255 64 64 64) ]
              [ el [ Font.bold ] (text t)
              , text d ]
        , c.chart ]
