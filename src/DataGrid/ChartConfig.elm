module DataGrid.ChartConfig exposing (..)

{-| Chart configuration type specifications, as well as defaults.
-}

import DataGrid.Internal.Defaults as Defaults



--------------------------------------------------------------------------------
-- Top-Level Types


type ChartCfg label
    = Std (StdChartCfg label)
    | Grid GridChartCfg
    | DefaultChartCfg


type ChartData label
    = BarChartData (StdSeries label)
    | LineChartData (List (StdSeries label))
    | FacetGridChartData (List (GridSeries GridPair))
    | StackedBarChartData (List (StdSeries label))
    | TreeGridChartData (List (GridSeries GridTriple))
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
    | FacetGridChartSpec
        { showHBar : Bool
        , barFillColor : String
        , labelAlign : Position
        }
    | StackedBarChartSpec
        { toggleSeries : Bool
        , toggleRelative : Bool
        , toggleHeight : Int
        }
    | TreeGridChartSpec
        { innerPad : Int
        , cellBaseFontSize : Int
        , cellMinFontSize : Int
        , toggleSeries : Bool
        , toggleHeight : Int
        }
    | DefaultSpec



--------------------------------------------------------------------------------
-- StdChart Data


{-| StdSeriesPair define data for working with StdChartCfg charts.
Logically, a single StdSeriesPair defines a single series (such as
a timeseries), paired with a string representin its name.

Each point in the series is a (label, Float) pair, where the conventioanl
representationmaps it to (x, y) coordinates in some form.

For consistency, all StdChartCfg charts take a list of StdSeriesPairs,
even if they only expect a single series (e.g. a simple bar chart).

-}
type alias StdSeries label =
    ( SeriesName, List ( label, Float ) )


type alias SeriesName =
    String



--------------------------------------------------------------------------------
-- StdChart Configs and Defaults


type alias StdChartCfg label =
    { w : Float
    , h : Float
    , axesSpec : AxesSpec
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
    , h = 400
    , axesSpec = defaultAxesSpec
    , pad = defaultPadding
    , chartSpec = DefaultSpec
    , dataAxisTicks = 5
    , showLabels = True
    , labelFormatter = \_ -> ""
    , tooltips = defaultTooltips
    , fontSpec = defaultFontSpec
    , legend = defaultLegend
    }


defaultAxesSpec =
    { yAxisMax = Nothing
    }


defaultBarChartSpec : ChartSpec
defaultBarChartSpec =
    BarChartSpec
        { fillColor = Defaults.rgbaToString Defaults.defaultFillColor
        , hoverColor = Defaults.rgbaToString Defaults.defaultHoverColor
        , showDistribution = True
        }


defaultStackedBarChartSpec : ChartSpec
defaultStackedBarChartSpec =
    StackedBarChartSpec
        { toggleSeries = True
        , toggleRelative = True
        , toggleHeight = 18
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
-- GridChart Data


{-| GridSeries define data for working with GridChartCfg charts.
-}
type alias GridSeries a =
    ( SeriesName, List ( FacetName, List a ) )


type alias GridPair =
    ( String, Float )


type alias GridTriple =
    ( String, Float, Float )


type alias FacetName =
    String



--------------------------------------------------------------------------------
-- GridChart Configs and Defaults


type alias GridChartCfg =
    { w : Float
    , h : Float
    , pad : Padding
    , baseFontSize : Int
    , chartSpec : ChartSpec
    , tooltips : Tooltips
    , fontSpec : FontSpec
    , legend : Legend
    }


defaultGridChartCfg : GridChartCfg
defaultGridChartCfg =
    { w = 900
    , h = 400
    , pad = defaultPadding
    , baseFontSize = 16
    , chartSpec = DefaultSpec
    , tooltips =
        { defaultTooltips
            | showHoverTooltips = True
        }
    , fontSpec = defaultFontSpec
    , legend = defaultLegend
    }


defaultFacetGridChartSpec : ChartSpec
defaultFacetGridChartSpec =
    FacetGridChartSpec
        { showHBar = True
        , barFillColor = Defaults.rgbaToString Defaults.defaultFillColor
        , labelAlign = Left
        }


defaultTreeGridChartSpec : ChartSpec
defaultTreeGridChartSpec =
    TreeGridChartSpec
        { innerPad = 2
        , cellBaseFontSize = 12
        , cellMinFontSize = 10
        , toggleSeries = True
        , toggleHeight = 18
        }



--------------------------------------------------------------------------------
-- Helper Types and Defaults


type alias AxesSpec =
    { yAxisMax : Maybe Float
    }


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
    , floatDp : Int
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
    { top = 10
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
    , floatDp = 2
    }


defaultFontSpec : FontSpec
defaultFontSpec =
    { textColor = Defaults.rgbToString Defaults.defaultTextColor
    , typeface = "Consolas"
    }


defaultLegend : Legend
defaultLegend =
    { show = True
    , location = Top
    }
