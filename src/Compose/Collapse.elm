module Compose.Collapse exposing (collapse)

import Html
import Html.App
import Html.Events as Events
import Compose.Program as P


type alias Model m =
    { collapsed : Bool
    , inner : m
    }


type Message m
    = Toggle
    | Other m


collapse : P.Program model msg -> P.Program (Model model) (Message msg)
collapse { init, subscriptions, view, update } =
    let
        ( m, c ) =
            init
    in
        { view = collapseView view
        , update = collapseUpdate update
        , init = ( { collapsed = False, inner = m }, Cmd.map Other c )
        , subscriptions = collapseSubscription subscriptions
        }


collapseSubscription : (model -> Sub msg) -> Model model -> Sub (Message msg)
collapseSubscription inner model =
    let
        innerSub =
            inner model.inner
    in
        Sub.map Other innerSub


collapseView : (model -> Html.Html msg) -> Model model -> Html.Html (Message msg)
collapseView inner model =
    if model.collapsed then
        Html.a [ Events.onClick Toggle ] [ Html.text "+" ]
    else
        Html.div []
            [ Html.a [ Events.onClick Toggle ] [ Html.text "-" ]
            , inner model.inner |> Html.App.map Other
            ]


collapseUpdate : (msg -> model -> ( model, Cmd msg )) -> Message msg -> Model model -> ( Model model, Cmd (Message msg) )
collapseUpdate inner msg model =
    case msg of
        Toggle ->
            ( { model | collapsed = not model.collapsed }, Cmd.none )

        Other m ->
            let
                ( newInner, c ) =
                    inner m model.inner
            in
                ( { model | inner = newInner }, Cmd.map Other c )
