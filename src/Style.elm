module Style exposing (Ids(..), Classes(..), css, id, class, classList)

import Html.CssHelpers
import Css exposing (..)
import Css.Elements as Elements
import Css.Namespace exposing (namespace)


type Ids
    = Header
    | Content
    | Footer


type Classes
    = UserHeader


namespaceName : String
namespaceName =
    "budget"


css : Stylesheet
css =
    (stylesheet << namespace namespaceName)
        [ Elements.body
            [ margin (px 0)
            , fontFamilies [ "Roboto" ]
            ]
        , (#) Header
            [ backgroundColor white
              -- , boxShadow4 (px 0) (px 0) (dp 6) (hex "000000")
            , border3 (dp 1) solid (hex "cccccc")
            , borderRadius (dp 3)
            , children
                [ Elements.h1 [ fontSize (dp 20), display inlineBlock ]
                ]
            ]
        , (.) UserHeader
            [ float right
            , fontSize (dp 20)
            ]
        ]


dp : Float -> Css.Px
dp x =
    px x


white : Color
white =
    hex "FFFFFF"


color500 : Color
color500 =
    hex "4CAF50"


{ id, class, classList } =
    Html.CssHelpers.withNamespace namespaceName
