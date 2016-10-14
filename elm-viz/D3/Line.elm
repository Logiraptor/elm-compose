module D3.Line exposing (..)

import D3.Format as Format
import String
import Svg
import Svg.Attributes


path : List ( Float, Float ) -> Svg.Svg a
path points =
    case points of
        ( x, y ) :: rest ->
            let
                d =
                    String.join " "
                        (("M " ++ (Basics.toString x) ++ " " ++ (Basics.toString y))
                            :: (List.map (\( x, y ) -> ("L " ++ (Basics.toString x) ++ " " ++ (Basics.toString y))) rest)
                        )
            in
                Svg.path [ Svg.Attributes.d d, Svg.Attributes.fill "none", Svg.Attributes.stroke "black" ] []

        [] ->
            Svg.text' [] [ Svg.text "No Data Available" ]


type Interpolator
    = Linear


type alias Line d =
    { x : d -> Float
    , y : d -> Float
    , interpolator : Interpolator
    }


line : (d -> Float) -> (d -> Float) -> Line d
line x y =
    { x = x, y = y, interpolator = Linear }


render : Line d -> List d -> String
render line data =
    case data of
        [] ->
            ""

        head :: tail ->
            ((Format.formatString "M ? ?" [ line.x head, line.y head ])
                ++ (renderPath line data)
            )


renderPath : Line d -> List d -> String
renderPath line data =
    case line.interpolator of
        Linear ->
            String.join " " (List.map (\d -> Format.formatString "L ? ?" [ line.x d, line.y d ]) data)



--line : List (Chart.ChartOption d) -> List d -> Svg.Svg a
--line options ds =
--    let
--        chart =
--            Chart.fromOptions options
--        xs =
--            List.map chart.x ds
--        ys =
--            List.map chart.y ds
--        xScale =
--            Scale.stretch xs ( 50, chart.width )
--        yScale =
--            Scale.stretch ys ( chart.height - 50, 0 )
--        x2s =
--            List.map (Scale.transform xScale) xs
--        y2s =
--            List.map (Scale.transform yScale) ys
--        linePath =
--            path (zip x2s y2s)
--    in
--        framed chart
--            <| Svg.g []
--                [ linePath
--                , Axis.xAxis chart xScale
--                , Axis.yAxis chart yScale
--                ]
--framed : Chart.Chart d -> Svg.Svg a -> Svg.Svg a
--framed chart child =
--    let
--        totalWidth =
--            chart.width + chart.margins.left + chart.margins.right
--        totalHeight =
--            chart.height + chart.margins.top + chart.margins.bottom
--    in
--        Svg.svg
--            [ Svg.Attributes.width (Format.px totalWidth)
--            , Svg.Attributes.height (Format.px totalHeight)
--            ]
--            [ Svg.g
--                [ Svg.Attributes.transform
--                    (Format.formatString "translate(? ?)"
--                        [ chart.margins.left
--                        , chart.margins.top
--                        ]
--                    )
--                ]
--                [ child
--                ]
--            ]
--zip : List a -> List b -> List ( a, b )
--zip =
--    List.map2 (,)
