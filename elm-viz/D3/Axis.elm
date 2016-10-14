module D3.Axis exposing (..)

import D3.Scale as Scale
import D3.Format as Format
import Svg
import Svg.Attributes


type Orientation
    = Top
    | Bottom
    | Left
    | Right


type Ticks
    = Exactly Int
    | EachPx Float


type alias Axis =
    { scale : Scale.Scale
    , ticks : Ticks
    , orientation : Orientation
    , tickFormat : Float -> String
    }


axis : Scale.Scale -> Axis
axis scale =
    { scale = scale, ticks = EachPx 100, orientation = Bottom, tickFormat = toString }


tickSize : Float -> Axis -> Axis
tickSize s axis =
    { axis | ticks = EachPx s }


ticks : Int -> Axis -> Axis
ticks s axis =
    { axis | ticks = Exactly s }


orient : Orientation -> Axis -> Axis
orient o axis =
    { axis | orientation = o }


tickFormat : (Float -> String) -> Axis -> Axis
tickFormat f axis =
    { axis | tickFormat = f }


render : Axis -> Svg.Svg a
render axis =
    let
        tickLength =
            5

        textSize =
            12

        size =
            abs axis.scale.range.range

        line x1 y1 x2 y2 =
            Svg.line
                [ Svg.Attributes.x1 (Format.px x1)
                , Svg.Attributes.y1 (Format.px y1)
                , Svg.Attributes.x2 (Format.px x2)
                , Svg.Attributes.y2 (Format.px y2)
                , Svg.Attributes.stroke "black"
                ]
                []

        spineCons =
            case axis.orientation of
                Left ->
                    line 0 0 0

                Right ->
                    line 0 0 0

                Top ->
                    Basics.flip (line 0 0) <| 0

                Bottom ->
                    Basics.flip (line 0 0) <| 0

        tickWithLabel x1 y1 x2 y2 label =
            let
                ( xOffset, yOffset, anchor, baseline ) =
                    case axis.orientation of
                        Left ->
                            ( -tickLength, 0, "end", "middle" )

                        Right ->
                            ( tickLength, 0, "start", "middle" )

                        Top ->
                            ( 0, -tickLength, "middle", "hanging" )

                        Bottom ->
                            ( 0, tickLength, "middle", "hanging" )
            in
                Svg.g []
                    [ line x1 y1 x2 y2
                    , Svg.text'
                        [ Svg.Attributes.x (Format.px (x1 + xOffset))
                        , Svg.Attributes.y (Format.px (y1 + yOffset))
                        , Svg.Attributes.fontSize (Format.px textSize)
                        , Svg.Attributes.textAnchor anchor
                        , Svg.Attributes.alignmentBaseline baseline
                        ]
                        [ Svg.text label ]
                    ]

        tickCons =
            case axis.orientation of
                Left ->
                    (\( x, lab ) -> tickWithLabel 0 x -tickLength x lab)

                Right ->
                    (\( x, lab ) -> tickWithLabel 0 x tickLength x lab)

                Top ->
                    (\( x, lab ) -> tickWithLabel x 0 x -tickLength lab)

                Bottom ->
                    (\( x, lab ) -> tickWithLabel x 0 x tickLength lab)

        spine =
            spineCons size

        tickValues =
            case axis.ticks of
                Exactly n ->
                    List.map ((*) (size / (toFloat n)) << toFloat) [0..n]

                EachPx x ->
                    List.map ((*) x) [0..(size / x)]

        pairWithLabel x =
            ( x, (axis.tickFormat (Scale.invert axis.scale x)) )

        ticks =
            List.map (tickCons << pairWithLabel) tickValues
    in
        Svg.g [] (spine :: ticks)
