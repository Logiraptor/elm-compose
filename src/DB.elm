port module DB exposing (persistent)

import Html.App as App
import Html
import Compose.Program as P
import Json.Decode as Decode
import Json.Decode as Encode


port put : Encode.Value -> Cmd msg


port updates : (Encode.Value -> msg) -> Sub msg


persistent : (model -> Encode.Value) -> (model -> Encode.Value -> model) -> P.Program model msg -> P.Program model (Msg msg)
persistent encoder decoder prog =
    { view = persistView prog.view
    , update = persistUpdates encoder decoder prog.update
    , subscriptions = persistSubscriptions prog.subscriptions
    , init = persistInit prog.init
    }


persistSubscriptions : (model -> Sub msg) -> model -> Sub (Msg msg)
persistSubscriptions inner model =
    let
        origSubscriptions =
            inner model
    in
        Sub.batch [ Sub.map Inner origSubscriptions, updates Commit ]


persistUpdates : (model -> Encode.Value) -> (model -> Encode.Value -> model) -> (msg -> model -> ( model, Cmd msg )) -> Msg msg -> model -> ( model, Cmd (Msg msg) )
persistUpdates encoder decoder inner msg model =
    case msg of
        Inner m ->
            let
                ( newModel, c ) =
                    inner m model
            in
                ( model, Cmd.batch [ newModel |> encoder |> put, Cmd.map Inner c ] )

        Commit m ->
            ( decoder model m, Cmd.none )


persistInit : ( model, Cmd msg ) -> ( model, Cmd (Msg msg) )
persistInit ( model, c ) =
    ( model, Cmd.map Inner c )


persistView : (model -> Html.Html msg) -> model -> Html.Html (Msg msg)
persistView inner model =
    let
        origView =
            inner model
    in
        App.map Inner origView


type Msg msg
    = Commit Encode.Value
    | Inner msg
