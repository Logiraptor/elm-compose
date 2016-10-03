module Compose.Tabs exposing (tabbed, with)

import Html
import Html.App as App
import Compose.Program as P


emptyProgram : P.Program LastTab Never
emptyProgram =
    P.beginnerProgram
        { view = \_ -> Html.i [] []
        , update = \_ -> \_ -> TabNever
        , model = TabNever
        }


type LastTab
    = TabNever


type Selection
    = Head
    | Tail


type alias Tab a b =
    ( a, b )


type alias NewModel t =
    { tab : t, names : List String, selected : Int }


type alias Model h t =
    { head : h, tail : t, selected : Selection }


type Message head tail
    = MsgHead head
    | MsgTail tail


tabbed : String -> P.Program model msg -> P.Program (Model model LastTab) (Message msg Never)
tabbed name prog =
    emptyProgram |> with name prog


with : String -> P.Program model msg -> P.Program oldModel oldMsg -> P.Program (Model model oldModel) (Message msg oldMsg)
with name prog otherTabs =
    { view = tabview name prog.view otherTabs.view
    , init = tabinit prog.init otherTabs.init
    , update = tabupdate prog.update otherTabs.update
    , subscriptions = tabsubscriptions prog.subscriptions otherTabs.subscriptions
    }


tabinit : ( model, Cmd msg ) -> ( otherModel, Cmd otherMsg ) -> ( Model model otherModel, Cmd (Message msg otherMsg) )
tabinit ( headModel, headCmd ) ( tailModel, tailCmd ) =
    ( { head = headModel, tail = tailModel, selected = Head }
    , Cmd.batch
        [ Cmd.map MsgHead headCmd
        , Cmd.map MsgTail tailCmd
        ]
    )


tabview : String -> (model -> Html.Html msg) -> (other -> Html.Html otherMsg) -> Model model other -> Html.Html (Message msg otherMsg)
tabview name view tailView model =
    let
        tabHeader =
            Html.h1 [] [ Html.text name ]
    in
        case model.selected of
            Head ->
                App.map MsgHead (Html.div [] [ tabHeader, view model.head ])

            Tail ->
                App.map MsgTail (tailView model.tail)


tabupdate : (msg -> model -> ( model, Cmd msg )) -> (otherMsg -> otherModel -> ( otherModel, Cmd otherMsg )) -> Message msg otherMsg -> Model model otherModel -> ( Model model otherModel, Cmd (Message msg otherMsg) )
tabupdate head tail msg model =
    case msg of
        MsgHead m ->
            let
                ( newHead, c ) =
                    head m model.head
            in
                ( { model | head = newHead }, Cmd.map MsgHead c )

        MsgTail m ->
            let
                ( newTail, c ) =
                    tail m model.tail
            in
                ( { model | tail = newTail }, Cmd.map MsgTail c )


tabsubscriptions : (model -> Sub msg) -> (otherModel -> Sub otherMsg) -> Model model otherModel -> Sub (Message msg otherMsg)
tabsubscriptions head tail model =
    let
        headSub =
            head model.head

        tailSub =
            tail model.tail
    in
        Sub.batch [ Sub.map MsgHead headSub, Sub.map MsgTail tailSub ]
