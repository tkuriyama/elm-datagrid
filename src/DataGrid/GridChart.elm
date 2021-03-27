module DataGrid.GridChart exposing (render)

{-| Render a a single Grid Chart.

-}

import Color exposing (Color)
import DataGrid.Config as Cfg
import DataGrid.Internal.Utils as Utils
import List.Extra as LE
import Scale exposing (BandScale, ContinuousScale, OrdinalScale)
import Shape
import String.Format
import TypedSvg exposing (g, line, rect, style, svg, text_)
import TypedSvg.Attributes
    exposing
        ( class
        , fill
        , stroke
        , textAnchor
        , transform
        , viewBox
        )
import TypedSvg.Attributes.InPx
    exposing
        ( strokeWidth
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types
    exposing
        ( AlignmentBaseline(..)
        , AnchorAlignment(..)
        , Paint(..)
        , Transform(..)
        )


--------------------------------------------------------------------------------




--------------------------------------------------------------------------------

render : Cfg.GridChartCfg -> List Cfg.GridSeries -> Svg msg
render cfg data = svg [] []
