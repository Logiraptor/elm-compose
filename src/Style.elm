module Style exposing (..)

import Css exposing (..)
import Css.Elements as Elements
import Css.Namespace exposing (namespace)


type Ids
    = Header


type Classes
    = Button


css : Stylesheet
css =
    (stylesheet << namespace "budget")
        [ Elements.body [ color (hex "ff0000") ]
        ]
