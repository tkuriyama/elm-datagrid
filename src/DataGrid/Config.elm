module DataGrid.Config exposing (..)

{-| Chart configuration type specifications, as well as defaults.
-}

import DataGrid.Internal.Defaults as Defaults



--------------------------------------------------------------------------------
-- Top-Level Types


type ChartCfg label
    = Std (StdChartCfg label)
    | DefaultChartCfg


type ChartData label
    = BarChartData (StdSeriesPair label)
    | LineChartData (StdSeriesPairs label)
    | DefaultData


type ChartSpec
    = BarChartSpec
        { fillColor : String
        , hoverColor : String
        , showDistribution : Bool
        }
    | LineChartSpec
        { showLineName : Bool
        , lineNameSize : Int
        , showVBar : Bool
        , toggleSeries : Bool
        , toggleRelative : Bool
        , toggleFirstDeriv : Bool
        , toggleHeight : Int
        }
    | DefaultSpec



--------------------------------------------------------------------------------
-- Data


type alias SeriesName =
    String


type alias StdSeriesPair label =
    ( SeriesName, List ( label, Float ) )


type alias StdSeriesPairs label =
    List (StdSeriesPair label)



--------------------------------------------------------------------------------
-- StdChart and Defaults


type alias StdChartCfg label =
    { w : Float
    , h : Float
    , pad : Padding
    , chartSpec : ChartSpec
    , dataAxisTicks : Int
    , showLabels : Bool
    , labelFormatter : label -> String
    , tooltips : Tooltips
    , fontSpec : FontSpec
    , legend : Legend
    }


defaultStdChartCfg : StdChartCfg label
defaultStdChartCfg =
    { w = 900
    , h = 450
    , pad = defaultPadding
    , chartSpec = DefaultSpec
    , dataAxisTicks = 5
    , showLabels = True
    , labelFormatter = \_ -> ""
    , tooltips = defaultTooltips
    , fontSpec = defaultFontSpec
    , legend = defaultLegend
    }


defaultBarChartSpec : ChartSpec
defaultBarChartSpec =
    BarChartSpec
        { fillColor = Defaults.rgbaToString Defaults.defaultFillColor
        , hoverColor = Defaults.rgbaToString Defaults.defaultHoverColor
        , showDistribution = True
        }


defaultLineChartSpec : ChartSpec
defaultLineChartSpec =
    LineChartSpec
        { showLineName = True
        , lineNameSize = 12
        , showVBar = True
        , toggleSeries = True
        , toggleRelative = True
        , toggleFirstDeriv = True
        , toggleHeight = 18
        }



--------------------------------------------------------------------------------
-- Helper Types and Defaults


type alias Padding =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


type alias Tooltips =
    { showTooltips : Bool
    , tooltipSize : Int
    , showLargeTooltips : Bool
    , largeTooltipSize : Int
    , showHoverTooltips : Bool
    , hoverTooltipSize : Int
    }


type alias FontSpec =
    { textColor : String
    , typeface : String
    }


type alias Legend =
    { show : Bool
    , location : Position
    }


type Position
    = Top
    | Right
    | Left
    | Bottom
    | Inline


defaultPadding : Padding
defaultPadding =
    { top = 30
    , right = 30
    , bottom = 30
    , left = 30
    }


defaultTooltips : Tooltips
defaultTooltips =
    { showTooltips = True
    , tooltipSize = 12
    , showLargeTooltips = False
    , largeTooltipSize = 20
    , showHoverTooltips = False
    , hoverTooltipSize = 16
    }


defaultFontSpec : FontSpec
defaultFontSpec =
    { textColor = Defaults.rgbToString Defaults.defaultTextColor
    , typeface = "Consolas"
    }


defaultLegend : Legend
defaultLegend =
    { show = False
    , location = Top
    }
