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

type alias BarChartConfig label =
    { w : Float
    , h : Float
    , padding : Float
    , orientation : Orientation
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


--------------------------------------------------------------------------------
-- Render

render : BarChartConfig label -> List (label, Float) -> Svg msg
render cfg model =
    let (dataEnd, labelEnd) = case cfg.orientation of
                                  Vertical -> (cfg.h, cfg.w)
                                  Horizontal -> (cfg.w, cfg.h)
        labels = Utils.fsts model
        dScale = dataScale dataEnd cfg.padding cfg.dataMin cfg.dataMax
        lScale = labelScale labelEnd cfg.padding labels
        tickCt = getTickCt cfg.dataAxisTicks cfg.dataMax
        draw = (drawBar cfg.orientation dataEnd cfg.padding cfg.labelFormat
                    dScale lScale)
    in svg
        [ viewBox 0 0 cfg.w cfg.h ]
        [ style [] [ text <| genStyle cfg.fillColor cfg.styleOverride ]
        , g [ transform [ Translate (cfg.padding - 1)
                              (dataEnd - cfg.padding) ] ]
            [ labelAxis cfg.labelFormat lScale ]
        , g [ transform [ Translate (cfg.padding - 1) cfg.padding ] ]
            [ dataAxis tickCt dScale ]
        , g [ transform [ Translate cfg.padding cfg.padding ]
            , class [ "series" ] ] <|
            List.map draw  model
        ]


drawBar : Orientation ->
          Float ->
          Float ->
          (label -> String) ->
          ContinuousScale Float ->
          BandScale label ->
          (label, Float) ->
          Svg msg
drawBar orientation dataEnd padding fmt dScale lScale pair =
    let f = case orientation of
                Vertical -> barV
                Horizontal -> barH
    in f dataEnd padding fmt dScale lScale pair


--------------------------------------------------------------------------------
-- Draw
barV : Float ->
       Float ->
       (label -> String) ->
       ContinuousScale Float ->
       BandScale label ->
       (label, Float) ->
       Svg msg
barV dataEnd padding fmt dScale lScale (txt, val) =
    g [ class [ "bar" ] ]
      [ rect
            [ x <| Scale.convert lScale txt
            , y <| Scale.convert dScale val
            , width <| Scale.bandwidth lScale
            , height <| dataEnd - Scale.convert dScale val - 2 * padding
            ]
            []
      , text_
            [ x <| Scale.convert (Scale.toRenderable fmt lScale) txt
            , y <| Scale.convert dScale val - 5
            , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat val ]
      ]
barH : Float ->
       Float ->
       (label -> String) ->
       ContinuousScale Float ->
       BandScale label ->
       (label, Float) ->
       Svg msg
barH dataEnd padding fmt dScale lScale (txt, val) =
    g [ class [ "bar" ] ]
      [ rect
            [ y <| Scale.convert lScale txt
            , x <| Scale.convert dScale val
            , height <| Scale.bandwidth lScale
            , width <| dataEnd - Scale.convert dScale val - 2 * padding
            ]
            []
      , text_
            [ y <| Scale.convert (Scale.toRenderable fmt lScale) txt
            , x <| Scale.convert dScale val - 5
            , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat val ]
      ]


--------------------------------------------------------------------------------
-- Scales and Axes

dataScale : Float -> Float -> Float -> Float -> ContinuousScale Float
dataScale end padding  dataMin dataMax =
     Scale.linear (end - 2 * padding, 0) (dataMin, dataMax)

dataAxis : Int -> ContinuousScale Float -> Svg msg
dataAxis tickCt dScale =
    Axis.left [ Axis.tickCount tickCt ] dScale

labelScale : Float -> Float -> List label -> BandScale label
labelScale end padding labels =
    let cfg = { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
    in Scale.band cfg (0, end - 2 * padding) labels

labelAxis : (label -> String) -> BandScale label -> Svg msg
labelAxis fmt lScale =
     Axis.bottom [] (Scale.toRenderable fmt lScale)

getTickCt : Maybe Int -> Float -> Int
getTickCt maybeDataAxisTicks dataMax =
    case maybeDataAxisTicks of
        (Just dataAxisTicks) -> dataAxisTicks
        Nothing -> min 10 (round dataMax)

--------------------------------------------------------------------------------
-- Style

defaultFillColor : String
defaultFillColor = "rgba(118, 214, 78, 0.8)"

defaultStyle : String -> String
defaultStyle fillColor =
    """
     .bar rect { fill: {{fillColor}}; }
     .bar text { display: none; }
     .bar:hover rect { fill: "rgb(118, 214, 78)"; }
     .bar:hover text { display: inline; }
     """
        |> String.Format.namedValue "fillColor" fillColor

genStyle : Maybe String -> Maybe String -> String
genStyle mFillColor mStyleOverride =
    case (mFillColor, mStyleOverride) of
        (Nothing, Nothing) -> defaultStyle defaultFillColor
        (Just fillColor, Nothing) -> defaultStyle fillColor
        (_, Just style) -> style

