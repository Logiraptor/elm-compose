module Viz.LineChart exposing (..)

import D3.Scale as Scale
import D3.Axis as Axis
import D3.Line as Line
import D3.Format as Format
import Svg
import Svg.Attributes


type alias LineChart d =
    { x : d -> Float
    , y : d -> Float
    , width : Float
    , height : Float
    , xAxisOpts : List (Axis.Axis -> Axis.Axis)
    , yAxisOpts : List (Axis.Axis -> Axis.Axis)
    }


chart : (d -> Float) -> (d -> Float) -> LineChart d
chart x y =
    { x = x
    , y = y
    , height = 300
    , width = 400
    , xAxisOpts = []
    , yAxisOpts = []
    }


render : LineChart d -> List d -> Svg.Svg a
render chart data =
    let
        margin =
            { top = 40, right = 40, bottom = 40, left = 40 }

        width =
            chart.width - margin.left - margin.right

        height =
            chart.height - margin.top - margin.bottom

        minX =
            Maybe.withDefault 0 <| List.minimum <| List.map chart.x data

        maxX =
            Maybe.withDefault 0 <| List.maximum <| List.map chart.x data

        x =
            Scale.linear ( minX, maxX ) ( 0, width )

        minY =
            Maybe.withDefault 0 <| List.minimum <| List.map chart.y data

        maxY =
            Maybe.withDefault 0 <| List.maximum <| List.map chart.y data

        y =
            Scale.linear ( minY, maxY ) ( height, 0 )

        xAxis =
            List.foldl (<|) (Axis.axis x) chart.xAxisOpts

        yAxis =
            List.foldl (<|) (Axis.axis y |> Axis.orient Axis.Left) chart.yAxisOpts

        line =
            Line.line (chart.x >> Scale.transform x) (chart.y >> Scale.transform y)
    in
        Svg.svg
            [ Svg.Attributes.width (Format.px (width + margin.left + margin.right))
            , Svg.Attributes.height (Format.px (height + margin.top + margin.bottom))
            ]
            [ Svg.g
                [ Svg.Attributes.transform
                    (Format.formatString "translate(?,?)"
                        [ margin.left, margin.top ]
                    )
                ]
                [ Svg.g
                    [ Svg.Attributes.transform
                        (Format.formatString "translate(0,?)"
                            [ height ]
                        )
                    ]
                    [ Axis.render xAxis
                    ]
                , Axis.render yAxis
                , Svg.path
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.d (Line.render line data)
                    ]
                    []
                ]
            ]


width : Float -> LineChart d -> LineChart d
width w c =
    { c | width = w }


height : Float -> LineChart d -> LineChart d
height h c =
    { c | height = h }


xAxis : (Axis.Axis -> Axis.Axis) -> LineChart d -> LineChart d
xAxis opt c =
    { c | xAxisOpts = opt :: c.xAxisOpts }


yAxis : (Axis.Axis -> Axis.Axis) -> LineChart d -> LineChart d
yAxis opt c =
    { c | yAxisOpts = opt :: c.yAxisOpts }
