module DataGrid.ChartGrid exposing ( ChartCell, LayoutCfg
                                   , chartGrid, defaultLayoutCfg )

{-| Create grids using elm-ui.

A grid is just an n x m matric of elements (typically of the same type),
where an element is an elm-ui `Element msg`.

-}

import Browser
import Element exposing ( Element, centerX, column, el, fill, height
                        , paragraph , padding, px, row, spacing, text
                        , width )
import Element.Font as Font
import Html exposing ( Html )

import DataGrid.Config as Cfg exposing ( ChartData(..) )
import DataGrid.Generic as Generic
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
    , showFirstDeriv : Bool
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
    }

type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    }

--------------------------------------------------------------------------------

chartGrid : LayoutCfg ->
            List (List (ChartCell label)) ->
            Program Flags (Model label) Msg
chartGrid cfg charts =
    Browser.element
        { init = init cfg charts
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

init : LayoutCfg ->
       List (List (ChartCell label)) ->
       Flags -> 
       (Model label, Cmd Msg)
init cfg charts =
    \flags ->
        ( { cfg = cfg
          , charts = charts
          , windowW = flags.windowWidth
          , windowH = flags.windowHeight
          }
        , Cmd.none
        )


--------------------------------------------------------------------------------

view : Model label -> Html msg
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
                 ([gridTitle, text "\n"] ++ rows) )

chartCell : LayoutCfg -> ChartCell label -> Element msg
chartCell cfg cell =
    let chart = Generic.render cell.chartCfg cell.chartData
    in column
        [ width fill ]
        [ title cell cfg.textColor cfg.cellBaseFontSize
        , chart |> Element.html ]

title : HasTitleDesc a -> Element.Color -> Int -> Element msg
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

--------------------------------------------------------------------------------
-- Update

update : Msg -> Model label -> (Model label, Cmd Msg)
update msg model = (model, Cmd.none)
