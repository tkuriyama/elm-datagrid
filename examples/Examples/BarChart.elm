module Examples.BarChart exposing (main)

import DateFormat
import Time
import TypedSvg exposing (svg)
import TypedSvg.Core exposing (Svg)

import DataGrid.BarChart exposing (Orientation(..), BarChartConfig, render)

--------------------------------------------------------------------------------

main : Svg msg
main =
    render cfg timeSeries

cfg : BarChartConfig Time.Posix
cfg = { w = 900
      , h = 450
      , padding = 30
      , orientation = Horizontal
      , labelFormat = dateFormat
      , dataMin = 0
      , dataMax = 5
      , dataAxisTicks = Just 5
      , fillColor = Nothing
      , styleOverride = Nothing
      }

timeSeries : List (Time.Posix, Float)
timeSeries =
    [ ( Time.millisToPosix 1448928000000, 2.5 )
    , ( Time.millisToPosix 1451606400000, 2 )
    , ( Time.millisToPosix 1452211200000, 3.5 )
    , ( Time.millisToPosix 1452816000000, 2 )
    , ( Time.millisToPosix 1453420800000, 3 )
    , ( Time.millisToPosix 1454284800000, 1 )
    , ( Time.millisToPosix 1456790400000, 1.2 )
    ]


--------------------------------------------------------------------------------

dateFormat : Time.Posix -> String
dateFormat =
    let fmtArgs = [ DateFormat.dayOfMonthFixed
                  , DateFormat.text " "
                  , DateFormat.monthNameAbbreviated ]
    in DateFormat.format fmtArgs Time.utc

