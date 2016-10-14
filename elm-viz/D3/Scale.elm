module D3.Scale exposing (..)


type alias Interval =
    { min : Float
    , max : Float
    , range : Float
    }


type alias Scale =
    { domain : Interval
    , range : Interval
    }


interval : ( Float, Float ) -> Interval
interval ( start, end ) =
    { min = start, max = end, range = end - start }


linear : ( Float, Float ) -> ( Float, Float ) -> Scale
linear domain range =
    { domain = interval domain, range = interval range }


stretch : List Float -> ( Float, Float ) -> Scale
stretch values range =
    let
        min =
            Maybe.withDefault 0 (List.minimum values)

        max =
            Maybe.withDefault min (List.maximum values)
    in
        linear ( min, max ) range


transform : Scale -> Float -> Float
transform { domain, range } x =
    (((x - domain.min) / domain.range) * range.range) + range.min


invert : Scale -> Float -> Float
invert { domain, range } =
    transform { domain = range, range = domain }
