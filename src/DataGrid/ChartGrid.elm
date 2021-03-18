module DataGrid.ChartGrid exposing ( ChartCell, LayoutCfg
                                   , chartGrid, defaultChartCell
                                   , defaultLayoutCfg, reindex )

{-| Create grids using elm-ui.

A grid is just an n x m matric of elements (typically of the same type),
where an element is an elm-ui `Element msg`.

-}

import Browser
import Element exposing ( Element, centerX, column, el, fill, height
                        , paragraph , padding, px, row, shrink, spacing
                        , text, width )
import Element.Font as Font
import Element.Input as Input
import Html exposing ( Html )

import DataGrid.Config as Cfg exposing ( ChartCfg(..), ChartData(..)
                                       , ChartSpec(..) )
import Internal.Defaults as Defaults
import Internal.Generic as Generic
import Internal.UI as UI
import Internal.Utils as Utils

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
    { index : Int
    , title : Maybe String
    , description : Maybe String
    , links : List String
    , chartCfg : Cfg.ChartCfg label
    , chartData : Cfg.ChartData label
    , hideSeries : List String
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

defaultChartCell : ChartCell label
defaultChartCell =
    { index = 0
    , title = Nothing
    , description = Nothing
    , links = []
    , chartCfg = Cfg.DefaultChartCfg
    , chartData = Cfg.DefaultData
    , hideSeries = []
    , showRelative = True
    , showFirstDeriv = True
    }


--------------------------------------------------------------------------------
-- Internal Types

type alias ChartGrid label
    = List (List (ChartCell label))

type alias Model label =
    { cfg : LayoutCfg
    , charts : ChartGrid label
    , windowH : Float
    , windowW : Float
    }

type Msg
    = ToggleHideSeries Int String Bool
    | ToggleRelative Int Bool
    | ToggleFirstDeriv Int Bool

type alias Flags =
    { windowWidth : Float
    , windowHeight : Float
    }

type alias HasTitleDesc a =
    { a |
      title : Maybe String
    , description : Maybe String
    }

type alias HasIndex a =
    { a |
      index : Int
    }


--------------------------------------------------------------------------------
-- Main

chartGrid : LayoutCfg -> ChartGrid label -> Program Flags (Model label) Msg
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
          , charts = reindex charts
          , windowW = flags.windowWidth
          , windowH = flags.windowHeight
          }
        , Cmd.none
        )

reindex : List (List (HasIndex a)) -> List (List (HasIndex a))
reindex xss =
    let colInds = List.map (\xs -> List.range 1 (List.length xs)) xss
        f r c x = { x |
                    index = r * 1000 + c
                  }
    in List.map2 Tuple.pair colInds xss
        |> List.indexedMap (\r (cs, xs) -> List.map2 (\c x -> f r c x) cs xs)


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
                 ([gridTitle, text "\n"] ++ rows) )

chartCell : LayoutCfg -> ChartCell label -> Element Msg
chartCell cfg cell =
    let chart = Generic.render cell.chartCfg cell.chartData
    in column
        [ width fill ]
        [ title cell cfg.textColor cfg.cellBaseFontSize
        , controls cfg cell
        , chart |> Element.html ]

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

controls : LayoutCfg -> ChartCell label -> Element Msg
controls cfg cell =
    let ((toggleSs, toggleRel, toggleFD), toggleH) = parseToggles cell.chartCfg
        attrs = [ width shrink
                , Font.size <| round (toFloat toggleH * 0.7)
                ]
    in row
        [ width shrink
        , spacing 2
        ]
        [ if not toggleRel then Element.none
          else
              Input.checkbox attrs
              { onChange = ToggleRelative cell.index
              , icon = UI.toggle toggleH "Relative" "Notional"
              , checked = not cell.showRelative
              , label = Input.labelHidden "Notional / Relative"
              }
        , if not toggleFD then Element.none
          else
              Input.checkbox attrs
              { onChange = ToggleFirstDeriv cell.index
              , icon = UI.toggle toggleH "1Δ " " 0Δ"
              , checked = not cell.showFirstDeriv
              , label = Input.labelHidden "0th Deriv / 1st Deriv"
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



--------------------------------------------------------------------------------
-- Update

update : Msg -> Model label -> (Model label, Cmd Msg)
update msg model =
    case msg of
        ToggleRelative i b ->
            ( { model | charts = mapGrid (updateRelative i b) model.charts }
            , Cmd.none
            )
        ToggleFirstDeriv i b ->
            ( { model | charts = mapGrid (updateFirstDeriv i b) model.charts }
            , Cmd.none
            )
        _ ->
            (model, Cmd.none)

mapGrid : (ChartCell label -> ChartCell label) ->
          ChartGrid label ->
          ChartGrid label
mapGrid f = List.map (\row -> List.map f row)

updateRelative : Int -> Bool -> ChartCell label -> ChartCell label
updateRelative i b cell =
    if cell.index /= i then cell else { cell | showRelative = xor b True }

updateFirstDeriv: Int -> Bool -> ChartCell label -> ChartCell label
updateFirstDeriv i b cell =
    if cell.index /= i then cell else { cell | showFirstDeriv = xor b True }
