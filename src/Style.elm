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
    = -- Structure
      CenterContent
      -- Skin
    | Card
    | Raised


namespaceName : String
namespaceName =
    "budget"


css : Stylesheet
css =
    (stylesheet << namespace namespaceName)
        [ Elements.body
            [ margin (px 0)
            , fontFamilies [ "Roboto" ]
            , backgroundColor (hex "eceff1")
            ]
        , (#) Header
            [ backgroundColor white
            , border3 (dp 1) solid (hex "cccccc")
            , children
                [ Elements.h1 [ display inlineBlock, fontSize (dp 20) ]
                ]
            ]
        , (.) CenterContent
            [ width (dp 960), margin auto, position relative ]
        , (.) Card
            [ backgroundColor white
            , borderRadius (dp 2)
            , margin (Css.rem 1)
            , padding (Css.rem 1)
            ]
        , (.) Raised
            [ boxShadow4 (px 0) (px 1) (px 3) (rgba 0 0 0 0.12)
            , boxShadow4 (px 0) (px 1) (px 2) (rgba 0 0 0 0.24)
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
