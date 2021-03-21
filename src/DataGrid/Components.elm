module DataGrid.Components exposing (boxPlot)

{- Individual SVG components for integration into charts.

-}

import Color exposing (Color)
import TypedSvg exposing (g, line, rect, svg)
import TypedSvg.Attributes exposing (class, stroke)
import TypedSvg.Attributes.InPx exposing (height, width, strokeWidth, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..))


--------------------------------------------------------------------------------

type alias BoxPlotEnv
    = { x : Float
      , y : Float
      , w : Float
      , p0 : Float
      , p25 : Float
      , p50 : Float
      , p75 : Float
      , p100 : Float
      }

boxPlot : BoxPlotEnv -> Svg msg
boxPlot env =
    g [ class [ "boxplot" ] ]
        [ rect [ x <| env.x
               , y <| env.y + env.p75
               , width <| env.w
               , height <| env.p25 - env.p75
               ]
              []
        , line [ x1 <| env.x + env.w / 2
               , x2 <| env.x + env.w / 2
               , y1 <| env.p100
               , y2 <| env.p75
               , strokeWidth 1
               , stroke <| Paint <| Color.rgb 0.25 0.25 0.25
               ]
              []
        , line [ x1 <| env.x
               , x2 <| env.x + env.w
               , y1 <| env.p100
               , y2 <| env.p100
               , strokeWidth 1.5
               , stroke <| Paint <| Color.rgb 0.25 0.25 0.25
               ]
              []
        , line [ x1 <| env.x + env.w / 2
               , x2 <| env.x + env.w / 2
               , y1 <| env.p0
               , y2 <| env.p25
               , strokeWidth 1
               , stroke <| Paint <| Color.rgb 0.25 0.25 0.25
               ]
              []
        , line [ x1 <| env.x
               , x2 <| env.x + env.w
               , y1 <| env.p0
               , y2 <| env.p0
               , strokeWidth 1.5
               , stroke <| Paint <| Color.rgb 0.25 0.25 0.25
               ]
              []
        , line [ x1 <| env.x
               , x2 <| env.x + env.w
               , y1 <| env.p50
               , y2 <| env.p50
               , strokeWidth 2
               , stroke <| Paint <| Color.rgb 0.25 0.25 0.25
               ]
              []
        ]
