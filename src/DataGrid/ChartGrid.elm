module DataGrid.ChartGrid exposing ( ChartCell, LayoutCfg
                                   , chartGrid, defaultChartCell
                                   , defaultLayoutCfg, reindex )

{-| Create grids using elm-ui.

A grid is just an n x m matric of elements (typically of the same type),
where an element is an elm-ui `Element msg`.

-}

import Browser
import Html exposing ( Html )

import DataGrid.ChartGrid.View as View
import DataGrid.ChartGrid.Types as Types exposing (..)
import DataGrid.Config as Cfg exposing ( ChartCfg(..), ChartData(..)
                                       , ChartSpec(..) )
import DataGrid.Internal.Defaults as Defaults
import DataGrid.Internal.Utils as Utils


--------------------------------------------------------------------------------
-- Exposed Types and Defaults

type alias LayoutCfg
    = Types.LayoutCfg

type alias ChartCell label
    = Types.ChartCell label

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
    , showRelative = False
    , showFirstDeriv = False
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

-- only works for rows of length < 1000
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
    View.view model


--------------------------------------------------------------------------------
-- Update

update : Msg -> Model label -> (Model label, Cmd Msg)
update msg model =
    case msg of
        ToggleSeries i name _ ->
            ( { model | charts = mapGrid (updateSeries i name) model.charts }
            , Cmd.none
            )
        ToggleRelative i b ->
            ( { model | charts = mapGrid (updateRelative i b) model.charts }
            , Cmd.none
            )
        ToggleFirstDeriv i b ->
            ( { model | charts = mapGrid (updateFirstDeriv i b) model.charts }
            , Cmd.none
            )

mapGrid : (ChartCell label -> ChartCell label) ->
          ChartGrid label ->
          ChartGrid label
mapGrid f = List.map (\row -> List.map f row)

updateSeries : Int -> String -> ChartCell label -> ChartCell label
updateSeries i name cell =
    if cell.index /= i then cell
    else { cell | hideSeries = Utils.toggleMember name cell.hideSeries }

updateRelative : Int -> Bool -> ChartCell label -> ChartCell label
updateRelative i b cell =
    if cell.index /= i then cell else { cell | showRelative = b }

updateFirstDeriv: Int -> Bool -> ChartCell label -> ChartCell label
updateFirstDeriv i b cell =
    if cell.index /= i then cell else { cell | showFirstDeriv = b }
