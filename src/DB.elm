port module DB exposing (..)

import Html
import Html.Events
import Compose.Program as P


port put : Model -> Cmd msg


port updates : (Model -> msg) -> Sub msg


prog : P.Program Model Msg
prog =
    { view = view
    , update = update
    , subscriptions = subscriptions
    , init = init
    }


type Msg
    = Set Model
    | Commit Model


type alias Model =
    { count : Int
    }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button
            [ Html.Events.onClick (Set { model | count = model.count + 1 }) ]
            [ Html.text "+" ]
        , Html.h1 []
            [ Html.text (toString model.count) ]
        , Html.button
            [ Html.Events.onClick (Set { model | count = model.count - 1 }) ]
            [ Html.text "-" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Set m ->
            ( model, put m )

        Commit m ->
            ( m, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { count = 0 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    updates Commit
