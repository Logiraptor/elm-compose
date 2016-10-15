module Compose.Program exposing (ProgramWithFlags, Program, BeginnerProgram, program, beginnerProgram)

import Html


type alias ProgramWithFlags flags model msg =
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html.Html msg
    }


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


beginnerProgram : BeginnerProgram model msg -> ProgramWithFlags () model msg
beginnerProgram prog =
    program
        { init = ( prog.model, Cmd.none )
        , update = noFxUpdate prog.update
        , subscriptions = noSub
        , view = prog.view
        }


program : Program model msg -> ProgramWithFlags () model msg
program prog =
    { init = \() -> prog.init
    , update = prog.update
    , subscriptions = prog.subscriptions
    , view = prog.view
    }


noSub : model -> Sub cmd
noSub _ =
    Sub.none


noFxUpdate : (msg -> model -> model) -> msg -> model -> ( model, Cmd msg )
noFxUpdate inner msg model =
    ( inner msg model, Cmd.none )
