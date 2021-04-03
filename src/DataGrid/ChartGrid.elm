module DataGrid.ChartGrid exposing
    ( chartGrid
    , defaultChartCell
    , defaultLayoutCfg
    , reindex
    )

{-| Create grids using elm-ui.

A grid is just an n x m matric of elements (typically of the same type),
where an element is an elm-ui `Element msg`.

-}

import Browser
import DataGrid.ChartConfig as Cfg
    exposing
        ( ChartCfg(..)
        , ChartData(..)
        , ChartSpec(..)
        )
import DataGrid.ChartGrid.View as View
import DataGrid.GridConfig exposing (..)
import DataGrid.Internal.Defaults as Defaults
import DataGrid.Internal.Utils as Utils
import Html exposing (Html)



--------------------------------------------------------------------------------
-- Exposed Types and Defaults


defaultLayoutCfg : LayoutCfg
defaultLayoutCfg =
    { w = Nothing
    , h = Nothing
    , colSpacing = 2
    , rowSpacing = 2
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


init :
    LayoutCfg
    -> ChartGrid label
    -> Flags
    -> ( Model label, Cmd Msg )
init cfg charts =
    \flags ->
        ( { cfg = cfg
          , charts = reindex 1 charts
          , windowW = flags.windowWidth
          , windowH = flags.windowHeight
          }
        , Cmd.none
        )


reindex : Int -> ChartGrid label -> ChartGrid label
reindex i grid =
    case grid of
        Column dims cells ->
            List.indexedMap (\i_ x -> reindex (i * 100 + i_) x) cells
                |> Column dims

        Row dims cells ->
            List.indexedMap (\i_ x -> reindex (i * 100 + i_) x) cells
                |> Row dims

        TabbedCell active cells ->
            let
                f i_ ( name, cell ) =
                    ( name, { cell | index = i * 100 + i_ } )
            in
            List.indexedMap f cells |> TabbedCell active

        Cell cell ->
            Cell { cell | index = i }



--------------------------------------------------------------------------------
-- View


view : Model label -> Html Msg
view model =
    View.view model



--------------------------------------------------------------------------------
-- Update


update : Msg -> Model label -> ( Model label, Cmd Msg )
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

        ActivateTab name ->
            ( { model | charts = updateActiveTab name model.charts }
            , Cmd.none
            )


mapGrid :
    (ChartCell label -> ChartCell label)
    -> ChartGrid label
    -> ChartGrid label
mapGrid f grid =
    case grid of
        Column dims cells ->
            Column dims <| List.map (mapGrid f) cells

        Row dims cells ->
            Row dims <| List.map (mapGrid f) cells

        TabbedCell active cells ->
            TabbedCell active <|
                List.map (\( name, cell ) -> ( name, f cell )) cells

        Cell cell ->
            Cell (f cell)


updateSeries : Int -> String -> ChartCell label -> ChartCell label
updateSeries i name cell =
    if cell.index /= i then
        cell

    else
        { cell | hideSeries = Utils.toggleMember name cell.hideSeries }


updateRelative : Int -> Bool -> ChartCell label -> ChartCell label
updateRelative i b cell =
    if cell.index /= i then
        cell

    else
        { cell | showRelative = b }


updateFirstDeriv : Int -> Bool -> ChartCell label -> ChartCell label
updateFirstDeriv i b cell =
    if cell.index /= i then
        cell

    else
        { cell | showFirstDeriv = b }


updateActiveTab : String -> ChartGrid label -> ChartGrid label
updateActiveTab name grid =
    case grid of
        Column dims cells ->
            Column dims <| List.map (updateActiveTab name) cells

        Row dims cells ->
            Row dims <| List.map (updateActiveTab name) cells

        TabbedCell active cells ->
            if List.member name (Utils.fsts cells) then
                TabbedCell name cells

            else
                TabbedCell active cells

        Cell cell ->
            Cell cell
