module SexpTests exposing (all)

import Sexp
import Expect
import Test exposing (..)
import Fuzz
import Regex
import Debug


removeSpaces : String -> String
removeSpaces =
    Regex.replace Regex.All (Regex.regex "\\s") (\_ -> "_")


sexpFuzzer : Fuzz.Fuzzer Sexp.Sexp
sexpFuzzer =
    Fuzz.frequencyOrCrash
        [ ( 0.25, Fuzz.map Sexp.Str Fuzz.string )
        , ( 0.25, Fuzz.map Sexp.Number Fuzz.int )
        , ( 0.25, Fuzz.map (removeSpaces >> Sexp.Atom) Fuzz.string )
        , ( 0.25, Fuzz.map Sexp.Group (Fuzz.list sexpFuzzer) )
        ]


toStringTestCases : List ( String, Sexp.Sexp )
toStringTestCases =
    [ ( "nil", Sexp.Group [] )
    , ( "empty string", Sexp.Str "" )
    , ( "non-empty string", Sexp.Str "bar" )
    , ( "escape string", Sexp.Str "foo\"bar" )
    , ( "zero", Sexp.Number 0 )
    , ( "one", Sexp.Number 1 )
    , ( "atom", Sexp.Atom "foo" )
    , ( "1-atom group", Sexp.Group [ Sexp.Atom "a" ] )
    , ( "2-atom group", Sexp.Group [ Sexp.Atom "a", Sexp.Atom "b" ] )
    , ( "nested group"
      , Sexp.Group
            [ Sexp.Atom "def"
            , Sexp.Group [ Sexp.Atom "lambda", Sexp.Atom "a", Sexp.Atom "b" ]
            ]
      )
    , ( "deeply nested group"
      , Sexp.Group
            [ Sexp.Atom "def"
            , Sexp.Group
                [ Sexp.Atom "lambda"
                , Sexp.Group [ Sexp.Atom "a", Sexp.Atom "b" ]
                , Sexp.Group
                    [ Sexp.Atom "if"
                    , Sexp.Group [ Sexp.Atom "=", Sexp.Atom "a", Sexp.Number 0 ]
                    , Sexp.Atom "a"
                    , Sexp.Atom "b"
                    ]
                ]
            ]
      )
    ]


all : Test
all =
    describe "S-expressions toString" <|
        -- transformation (Sexp.sexpToString >> (Sexp.parse >> Maybe.withDefault (Sexp.Group []))) toStringTestCases
        [ fuzz sexpFuzzer "toString / parse goes both ways" <|
            \sexp ->
                let
                    str =
                        Sexp.sexpToString (Debug.log "sexp" sexp)

                    reparsed =
                        Sexp.parse str
                in
                    case reparsed of
                        Nothing ->
                            Expect.fail "parsing failed"

                        Just x ->
                            Expect.equal sexp x
        ]


transformation : (a -> a) -> List ( String, a ) -> List Test.Test
transformation f l =
    List.map
        (\( name, str ) ->
            test name <|
                \() ->
                    f str
                        |> Expect.equal str
        )
        l
