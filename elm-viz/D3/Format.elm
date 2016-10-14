module D3.Format exposing (..)

import Regex


px : x -> String
px x =
    (toString x) ++ "px"


formatDelim : Regex.Regex
formatDelim =
    Regex.regex "\\?"


formatString : String -> List x -> String
formatString fmt args =
    case args of
        [] ->
            fmt

        head :: rest ->
            formatString (Regex.replace (Regex.AtMost 1) formatDelim (\_ -> (toString head)) fmt) rest
