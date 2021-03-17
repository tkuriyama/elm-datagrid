module DataGrid.Layout exposing ( ChartCell, LayoutCfg
                                , defaultLayoutCfg, main )

{-| Create grids using elm-ui.

A grid is just an n x m matric of elements (typically of the same type),
where an element is an elm-ui `Element msg`.

-}

import Browser
import Element exposing ( Element, centerX, column, el, fill, height
                        , paragraph , padding, px, row, spacing, text
                        , width )
import Element.Font as Font
import Html exposing (Html)y

import DataGrid.Config as Cfg
import Internal.Defaults as Defaults


--------------------------------------------------------------------------------
-- Exposed Types

type alias LayoutCfg =
    { w : Maybe Int
    , h : Maybe Int
    , colSpacing : Int
    , rowSpacing : Int
    , padding : Int
    , title : Maybe String
    , description : Maybe String
    , links : List String
    , textColor : Element.Color
    , typeface : String
    , gridBaseFontSize : Int
    , cellBaseFontSize : Int
    }

type alias ChartCell label =
    { title : Maybe String
    , description : Maybe String
    , links : List String
    , chartCfg : Cfg.ChartCfg label
    , chartData : Cfg.ChartData label
    , showSeries : List String
    , showRelative : Bool
    , showFirstderiv : Bool
    }

defaultLayoutCfg : LayoutCfg
defaultLayoutCfg =
    { w = Just 1800
    , h = Nothing
    , colSpacing = 0
    , rowSpacing = 5
    , padding = 5
    , title = Nothing
    , description = Nothing
    , links = []
    , textColor = Defaults.rgbToElmUI Defaults.defaultTextColor
    , typeface = Defaults.defaultTypeface
    , gridBaseFontSize = 22
    , cellBaseFontSize = 16
    }


--------------------------------------------------------------------------------
-- Internal Types

type alias Model label =
    { cfg : LayoutCfg
    , charts : List (List (ChartCell label))
    , windowH : Float
    , windowW : Float
    }

type Msg
    = UpdateShowSeries (List String)
    | UpdateShowRelative Bool
    | UpdateShowFirstDeriv Bool

type alias HasTitleDesc a =
    { a |
      title : Maybe String
    , description : Maybe String
    }git sta


--------------------------------------------------------------------------------

main : Program flags model msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

init : flags -> (Model label, Cmd Msg)
init flags =
    { cfg = defaultLayoutCfg
    , charts = []
    , windowW = flags.windowWidth
    , windowH = flags.windowHeight
    }


--------------------------------------------------------------------------------

view : Model label -> Html msg
view = html [] []

--------------------------------------------------------------------------------
-- Update

update : Msg -> Model -> (Model label, Cmd Msg)
update msg model = Cmd.none

--------------------------------------------------------------------------------
-- Chart Grid

chartGrid : LayoutConfig -> List (List (Chart msg)) -> Html msg
chartGrid cfg xss =
    let rows = List.map genRow xss
        genRow xs = row [ width fill, spacing cfg.colSpacing ]
                        (List.map (chartCell cfg) xs)
        gridTitle = title cfg cfg.textColor cfg.gridBaseFontSize
        tf = Maybe.withDefault Defaults.defaultTypeface cfg.typeface
    in
        Element.layout
            [ Font.family [ Font.typeface tf, Font.sansSerif ]
            , padding cfg.padding
            ]
            ( column
                 [ centerX, width <| px cfg.w, spacing cfg.rowSpacing ]
                 ([gridTitle, text "\n"] ++ rows) )

chartCell : LayoutConfig -> Chart msg -> Element msg
chartCell cfg c =
    column
        [ width fill ]
        [ title c cfg.textColor cfg.cellBaseFontSize
        , c.chart ]


--------------------------------------------------------------------------------
-- Helpers

title : HasTitleDesc a -> Maybe Element.Color -> Int -> Element msg
title r mColor baseFont =
    let t = Maybe.withDefault "" r.title
        d = Maybe.withDefault "" r.description
        fc = Maybe.withDefault (Defaults.rgbToElmUI Defaults.defaultTextColor)
             mColor
        smallFont = round <| toFloat baseFont * 0.8
    in paragraph
        [ Font.color fc ]
        [ el [ Font.bold, Font.size baseFont ] (text t)
        , text " | "
        , el [ Font.size smallFont ] (text d)
        ]
