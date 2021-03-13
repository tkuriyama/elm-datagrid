module DataGrid.LineShareChart exposing ( render )

{-| Render a a single Line Share chart.
-}

import Axis
import Scale exposing ( BandScale, ContinuousScale, defaultBandConfig )
import String.Format
import TypedSvg exposing ( g, rect, style, svg, text_ )
import TypedSvg.Attributes exposing ( alignmentBaseline, class, textAnchor
                                    , transform, viewBox )
import TypedSvg.Attributes.InPx exposing ( height, width, x, y )
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing ( AlignmentBaseline(..), AnchorAlignment(..)
                               , Transform(..) )

import DataGrid.Config as Cfg
import Internal.Defaults as Defaults
import Internal.Utils as Utils


--------------------------------------------------------------------------------
-- BarChartConfig is converted to ChartEnv for internal use

--------------------------------------------------------------------------------
-- StdChartfg is converted to ChartEnv for internal use

type alias ChartEnv label =
    { w: Float
    , h : Float
    , pad : Cfg.Padding
    , dataScale : ContinuousScale Float
    , labelScale : BandScale label
    , labelShow : Bool
    , labelFmt : label -> String
    , dataTickCt : Int
    , tooltips : Cfg.Tooltips
    , legend : Cfg.Legend
    , style : String
    }


--------------------------------------------------------------------------------


render = ""
