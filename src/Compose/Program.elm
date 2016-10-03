module Compose.Program exposing (Program, BeginnerProgram, beginnerProgram)

import Html


type alias Program model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html.Html msg
    }


type alias BeginnerProgram model msg =
    { model : model
    , update : msg -> model -> model
    , view : model -> Html.Html msg
    }


beginnerProgram : BeginnerProgram model msg -> Program model msg
beginnerProgram prog =
    { init = ( prog.model, Cmd.none )
    , update = noFxUpdate prog.update
    , subscriptions = noSub
    , view = prog.view
    }


noSub : model -> Sub cmd
noSub _ =
    Sub.none


noFxUpdate : (msg -> model -> model) -> msg -> model -> ( model, Cmd msg )
noFxUpdate inner msg model =
    ( inner msg model, Cmd.none )
