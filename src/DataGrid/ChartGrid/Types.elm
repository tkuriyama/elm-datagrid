module DataGrid.ChartGrid.Types exposing (..)

import Element

import DataGrid.Config as Cfg exposing ( ChartCfg (..), ChartData (..)
                                       , ChartSpec (..) )

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
    = ToggleSeries Int String Bool
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
    , links : List String
    }

type alias HasIndex a =
    { a |
      index : Int
    }
