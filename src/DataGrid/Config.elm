module DataGrid.Config exposing (..)

{-| Chart configuration type specifications, as well as defaults.

-}

import Internal.Defaults as Defaults


--------------------------------------------------------------------------------
-- Type Definitions

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

type ChartSpec
    = BarChartSpec { fillColor : String
                   , hoverColor : String
                   }
    | LineChartSpec { showLineName : Bool
                    , lineNameSize : Int
                    , showVBar : Bool
                    }
    | LineShareChartSpec { toggleSeries : Bool
                         , toggleRelative : Bool
                         , toggleFirstDeriv : Bool
                         }
    | DefaultSpec


--------------------------------------------------------------------------------
-- Helper Types

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
--------------------------------------------------------------------------------
-- StdChartCfg Defaults

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
    }

defaultLineChartSpec : ChartSpec
defaultLineChartSpec =
    LineChartSpec
    { showLineName = True
    , lineNameSize = 12
    , showVBar = False
    }

defaultLineShareChartSpec : ChartSpec
defaultLineShareChartSpec =
    LineShareChartSpec
    { toggleSeries = False
    , toggleRelative = True
    , toggleFirstDeriv = True
    }

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
