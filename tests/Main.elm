port module Main exposing (..)

import SexpTests
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Value
main =
    run emit SexpTests.all


port emit : ( String, Value ) -> Cmd msg
