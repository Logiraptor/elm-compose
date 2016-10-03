module Sexp exposing (Sexp(..), sexpToString, parse)

import Regex
import String
import Parser
import Parser.Char
import Parser.Number


type Sexp
    = Str String
    | Atom String
    | Number Int
    | Group (List Sexp)


quoteRegexp : Regex.Regex
quoteRegexp =
    Regex.regex "\\\""


sexpToString : Sexp -> String
sexpToString sexp =
    let
        indented depth sexp =
            case sexp of
                Str s ->
                    let
                        escaped =
                            Regex.replace Regex.All quoteRegexp (\_ -> "\\\"") s
                    in
                        "\"" ++ escaped ++ "\""

                Number n ->
                    toString n

                Atom a ->
                    a

                Group l ->
                    let
                        partLengths =
                            List.map approxLength l

                        totalLength =
                            List.sum partLengths

                        ( separator, newDepth ) =
                            if totalLength > 15 then
                                ( "\n" ++ String.repeat (depth + 1) "    ", depth + 1 )
                            else
                                ( " ", depth )

                        parts =
                            List.map (indented newDepth) l

                        inner =
                            String.join separator parts
                    in
                        "(" ++ inner ++ ")"
    in
        indented 0 sexp


approxLength : Sexp -> Int
approxLength sexp =
    case sexp of
        Atom a ->
            String.length a

        Number n ->
            toString n
                |> String.length

        Str s ->
            String.length s
                |> (+) 2

        Group l ->
            List.map approxLength l
                |> List.sum
                |> (+) 2


parse : String -> Maybe Sexp
parse s =
    let
        r =
            Parser.parse sexpParser s
    in
        case r of
            Ok s ->
                Just s

            Err e ->
                Just (Str e)


sexpParser : Parser.Parser Sexp
sexpParser =
    let
        whitespace =
            Parser.choice
                [ Parser.symbol ' '
                , Parser.symbol '\t'
                , Parser.symbol '\n'
                ]
                |> Parser.many

        atomParser =
            Parser.satisfy (\c -> c /= ' ' && c /= '\t' && c /= '\n')
                |> Parser.many
                |> Parser.map String.fromList
                |> Parser.map Atom

        numberParser =
            Parser.map Number Parser.Number.integer

        strParser =
            Parser.satisfy (\c -> c /= '"')
                |> Parser.many
                |> Parser.map String.fromList
                |> Parser.Char.quoted
                |> Parser.map Str

        groupParser =
            Parser.separatedBy sexpParser whitespace
                |> Parser.Char.parenthesized
                |> Parser.map Group
    in
        Parser.choice [ atomParser, numberParser, strParser, groupParser ]
