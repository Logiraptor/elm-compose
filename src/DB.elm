port module DB exposing (..)

import Html.App as App
import Html
import Compose.Program as P
import Json.Decode as Decode
import Json.Decode as Encode


port put : Encode.Value -> Cmd msg


port updates : (Encode.Value -> msg) -> Sub msg


persistent : P.Program model msg -> (model -> Encode.Value) -> (Encode.Value -> model) -> P.Program model (Msg model msg)
persistent prog encoder decoder =
    { view = persistView prog.view
    , update = persistUpdates encoder prog.update
    , subscriptions = persistSubscriptions prog.subscriptions decoder
    , init = persistInit prog.init
    }


persistSubscriptions : (model -> Sub msg) -> (Encode.Value -> model) -> model -> Sub (Msg model msg)
persistSubscriptions inner decoder model =
    let
        origSubscriptions =
            inner model
    in
        Sub.batch [ Sub.map Inner origSubscriptions, updates (decoder >> Commit) ]


persistUpdates : (model -> Encode.Value) -> (msg -> model -> ( model, Cmd msg )) -> Msg model msg -> model -> ( model, Cmd (Msg model msg) )
persistUpdates encoder inner msg model =
    case msg of
        Inner m ->
            ( model, model |> encoder |> put )

        Commit m ->
            ( m, Cmd.none )


persistInit : ( model, Cmd msg ) -> ( model, Cmd (Msg model msg) )
persistInit ( model, c ) =
    ( model, Cmd.map Inner c )


persistView : (model -> Html.Html msg) -> model -> Html.Html (Msg model msg)
persistView inner model =
    let
        origView =
            inner model
    in
        App.map Inner origView


type Msg model msg
    = Commit model
    | Inner msg
