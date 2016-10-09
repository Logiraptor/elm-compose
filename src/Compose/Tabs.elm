module Compose.Tabs exposing (tabbed, with, TabView, TabLabel(..), end)

import Html
import Html.Events as Events
import Html.App as App
import Compose.Program as P
import Debug


emptyProgram : TabProgram (Model LastTab LastTab) Never
emptyProgram =
    { bodyview = \_ -> Html.i [] []
    , update = \_ -> \_ -> ( { tab = ( TabNever, TabNever ), names = [], selected = 0 }, Cmd.none )
    , init = ( { tab = ( TabNever, TabNever ), names = [], selected = 0 }, Cmd.none )
    , subscriptions = \_ -> Sub.none
    }


type LastTab
    = TabNever


type alias TabName =
    String


type TabLabel msg
    = Selected TabName
    | Unselected TabName msg


type alias TabView msg =
    List (TabLabel msg) -> Html.Html msg -> Html.Html msg


type alias Tab a b =
    ( a, b )


type alias Model a b =
    { tab : Tab a b, names : List TabName, selected : Int }


type Message head tail
    = MsgHead head
    | MsgTail tail
    | Select Int


type alias TabProgram model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , bodyview : model -> Html.Html msg
    }


tabbed : TabName -> P.Program model msg -> TabProgram (Model model (Tab LastTab LastTab)) (Message msg Never)
tabbed name prog =
    emptyProgram |> with name prog


with : TabName -> P.Program model msg -> TabProgram (Model a b) oldMsg -> TabProgram (Model model (Tab a b)) (Message msg oldMsg)
with name prog otherTabs =
    { bodyview = tabview prog.view otherTabs.bodyview
    , init = tabinit name prog.init otherTabs.init
    , update = tabupdate prog.update otherTabs.update
    , subscriptions = tabsubscriptions prog.subscriptions otherTabs.subscriptions
    }


end : TabView (Message msg b) -> TabProgram (Model model a) (Message msg b) -> P.Program (Model model a) (Message msg b)
end view prog =
    { view = wrapview view prog.bodyview
    , init = prog.init
    , subscriptions = prog.subscriptions
    , update = prog.update
    }


wrapview : TabView (Message msg b) -> (Model model a -> Html.Html (Message msg b)) -> Model model a -> Html.Html (Message msg b)
wrapview tabview view model =
    let
        label i n =
            if i == model.selected then
                Selected n
            else
                Unselected n (Select i)

        labels =
            List.indexedMap label model.names
    in
        tabview labels (view model)


tabinit : TabName -> ( model, Cmd msg ) -> ( Model a b, Cmd c ) -> ( Model model (Tab a b), Cmd (Message msg c) )
tabinit name ( headModel, headCmd ) ( tailModel, tailCmd ) =
    ( { tab = ( headModel, tailModel.tab ), selected = 0, names = name :: tailModel.names }
    , Cmd.batch
        [ Cmd.map MsgHead headCmd
        , Cmd.map MsgTail tailCmd
        ]
    )


tabview : (model -> Html.Html msg) -> (Model a b -> Html.Html c) -> Model model (Tab a b) -> Html.Html (Message msg c)
tabview headView tailView model =
    case model.selected of
        0 ->
            App.map MsgHead <| headView (fst model.tab)

        _ ->
            App.map MsgTail <| tailView { names = [], selected = model.selected - 1, tab = snd model.tab }


tabupdate : (msg -> model -> ( model, Cmd msg )) -> (c -> Model a b -> ( Model a b, Cmd c )) -> Message msg c -> Model model (Tab a b) -> ( Model model (Tab a b), Cmd (Message msg c) )
tabupdate head tail msg model =
    case msg of
        MsgHead m ->
            let
                r =
                    head m (fst model.tab)

                ( newHead, c ) =
                    r
            in
                ( { model | tab = ( newHead, snd model.tab ) }, Cmd.map MsgHead c )

        MsgTail m ->
            let
                r =
                    tail m { model | selected = model.selected - 1, tab = snd model.tab }

                ( newTail, c ) =
                    r
            in
                ( { model | tab = ( fst model.tab, newTail.tab ) }, Cmd.map MsgTail c )

        Select i ->
            ( { model | selected = i }, Cmd.none )


tabsubscriptions : (model -> Sub msg) -> (Model a b -> Sub c) -> Model model (Tab a b) -> Sub (Message msg c)
tabsubscriptions head tail model =
    let
        headSub =
            head (fst model.tab)

        tailSub =
            tail { names = model.names, selected = model.selected - 1, tab = snd model.tab }
    in
        Sub.batch [ Sub.map MsgHead headSub, Sub.map MsgTail tailSub ]
