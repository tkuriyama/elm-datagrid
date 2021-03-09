module DataGrid.BarChart exposing (Orientation(..), BarChartConfig, render)

{-| Module for rendering a single Barchart, which may be specified as vertically
or horizontally oriented.

|-}

import Axis
import DateFormat
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import String.Format
import Time
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))

import Internal.Utils as Utils exposing (..)


--------------------------------------------------------------------------------
-- BarChartConfig is converted to ChartEnv for internal use

type alias BarChartConfig label =
    { w : Float
    , h : Float
    , padding : Float
    , orientation: Orientation
    , labelFormat : label -> String
    , dataMin : Float
    , dataMax : Float
    , dataAxisTicks : Maybe Int
    , fillColor : Maybe String
    , styleOverride : Maybe String
    }

type Orientation
    = Vertical
    | Horizontal

type alias ChartEnv label =
    { w: Float
    , h : Float
    , pad : Float
    , orient : Orientation
    , dataEnd: Float
    , labelEnd: Float
    , dataScale : ContinuousScale Float
    , labelScale : BandScale label
    , fmt : label -> String
    , style : String
    , tickCt : Int
    }


--------------------------------------------------------------------------------
-- Render

render : BarChartConfig label -> List (label, Float) -> Svg msg
render cfg model =
    let env = genChartEnv cfg model
    in case cfg.orientation of
           Vertical -> renderV env model
           Horizontal -> renderH env model

renderV : ChartEnv label -> List (label, Float) -> Svg msg
renderV env model =
    svg [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        -- Label Axis
        , g [ transform [ Translate (env.pad - 1) (env.h - env.pad)] ]
            [ labelAxis env.fmt env.labelScale env.orient ]
        -- Data Axis
        , g [ transform [ Translate (env.pad - 1) env.pad ] ]
            [ dataAxis env.tickCt env.dataScale env.orient ]
        -- Data Elements
        , g [ transform [ Translate env.pad env.pad ], class [ "series" ] ] <|
            List.map (barV env) model
        ]


renderH : ChartEnv label -> List (label, Float) -> Svg msg
renderH env model =
    svg [ viewBox 0 0 env.w env.h ]
        [ style [] [ text <| env.style ]
        -- Label Axis
        , g [ transform [ Translate (env.pad * 2 - 1) env.pad] ]
            [ labelAxis env.fmt env.labelScale env.orient ]
        -- Data Axis
        , g [ transform [ Translate (env.pad * 2 - 1) env.pad ] ]
            [ dataAxis env.tickCt env.dataScale env.orient ]
        -- Data Elements
        , g [ transform [ Translate env.pad env.pad ], class [ "series" ] ] <|
            List.map (barH env) model
        ]

genChartEnv : BarChartConfig label -> List (label, Float) -> ChartEnv label
genChartEnv cfg model =
    let (dEnd, lEnd) = case cfg.orientation of
                           Vertical -> (cfg.h, cfg.w)
                           Horizontal -> (cfg.w, cfg.h)
        dScale = genDataScale dEnd cfg.padding cfg.dataMin cfg.dataMax cfg.orientation
        lScale = genLabelScale lEnd cfg.padding <| Utils.fsts model
    in { w = cfg.w
       , h = cfg.h
       , dataEnd = dEnd
       , pad = cfg.padding
       , orient = cfg.orientation
       , labelEnd = lEnd
       , dataScale = dScale
       , labelScale = lScale
       , fmt = cfg.labelFormat
       , style = genStyle cfg.fillColor cfg.styleOverride
       , tickCt = getTickCt cfg.dataAxisTicks cfg.dataMax
       }


--------------------------------------------------------------------------------
-- Draw

barV : ChartEnv label -> (label, Float) -> Svg msg
barV env (lbl, val) =
    g [ class [ "bar" ] ]
      [ rect
            [ x <| Scale.convert env.labelScale lbl
            , y <| Scale.convert env.dataScale val
            , width <| Scale.bandwidth env.labelScale
            , height <| env.dataEnd -
                Scale.convert env.dataScale val - (2 * env.pad)
            ]
            []
      , text_
            [ x <| Scale.convert (Scale.toRenderable env.fmt env.labelScale) lbl
            , y <| Scale.convert env.dataScale val - 5
            , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat val ]
      ]

barH : ChartEnv label -> (label, Float) -> Svg msg
barH env (lbl, val) =
    g [ class [ "bar" ] ]
      [ rect
            [ x <| env.pad -- Scale.convert env.labelScale lbl
            , y <| Scale.convert env.labelScale lbl -- Scale.convert env.dataScale val
            , width <| env.dataEnd -
                Scale.convert env.dataScale val - (2 * env.pad)
            , height <| Scale.bandwidth env.labelScale
            ]
            []
      , text_
            [ x <| Scale.convert env.dataScale val - 5
            , y <| Scale.convert (Scale.toRenderable env.fmt env.labelScale) lbl
            , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat val ]
      ]

--------------------------------------------------------------------------------
-- Scales and Axes

genDataScale : Float ->
               Float ->
               Float ->
               Float ->
               Orientation ->
               ContinuousScale Float
genDataScale end padding  dataMin dataMax orientation =
    case orientation of
        Vertical -> Scale.linear (end - 2 * padding, 0) (dataMin, dataMax)
        Horizontal -> Scale.linear (0, end - 2 * padding) (dataMin, dataMax)

dataAxis : Int -> ContinuousScale Float -> Orientation -> Svg msg
dataAxis tickCt dScale orientation =
    case orientation of
        Vertical -> Axis.left [ Axis.tickCount tickCt ] dScale
        Horizontal -> Axis.top [ Axis.tickCount tickCt ] dScale

genLabelScale : Float -> Float -> List label -> BandScale label
genLabelScale end padding labels =
    let cfg = { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in Scale.band cfg (0, end - 2 * padding) labels

labelAxis : (label -> String) -> BandScale label -> Orientation -> Svg msg
labelAxis fmt lScale orientation =
    case orientation of
        Vertical -> Axis.bottom [] (Scale.toRenderable fmt lScale)
        Horizontal -> Axis.left [] (Scale.toRenderable fmt lScale)

getTickCt : Maybe Int -> Float -> Int
getTickCt maybeDataAxisTicks dataMax =
    case maybeDataAxisTicks of
        (Just dataAxisTicks) -> dataAxisTicks
        Nothing -> min 10 (round dataMax)

--------------------------------------------------------------------------------


defaultFillColor : String
defaultFillColor = "rgba(118, 214, 78, 0.8)"

defaultStyle : String -> String
defaultStyle fillColor =
    """
     .bar rect { fill: {{fillColor}}; }
     .bar text { display: none; }
     .bar:hover rect { fill: darkgreen; }
     .bar:hover text { display: inline; }
     """
        |> String.Format.namedValue "fillColor" fillColor

genStyle : Maybe String -> Maybe String -> String
genStyle mFillColor mStyleOverride =
    case (mFillColor, mStyleOverride) of
        (Nothing, Nothing) -> defaultStyle defaultFillColor
        (Just fillColor, Nothing) -> defaultStyle fillColor
        (_, Just style) -> style

