module Compose.Tabs exposing (tabbed, with)

import Html
import Html.Events as Events
import Html.App as App
import Compose.Program as P
import Debug


emptyProgram : P.Program (Model LastTab LastTab) Never
emptyProgram =
    P.beginnerProgram
        { view = \_ -> Html.i [] []
        , update = \_ -> \_ -> { tab = ( TabNever, TabNever ), names = [], selected = 0 }
        , model = { tab = ( TabNever, TabNever ), names = [], selected = 0 }
        }


type LastTab
    = TabNever


type alias Tab a b =
    ( a, b )


type alias Model a b =
    { tab : Tab a b, names : List String, selected : Int }


type Message head tail
    = MsgHead head
    | MsgTail tail
    | Select Int


tabbed : String -> P.Program model msg -> P.Program (Model model (Tab LastTab LastTab)) (Message msg Never)
tabbed name prog =
    emptyProgram |> with name prog


with : String -> P.Program model msg -> P.Program (Model a b) oldMsg -> P.Program (Model model (Tab a b)) (Message msg oldMsg)
with name prog otherTabs =
    { view = tabview prog.view otherTabs.view
    , init = tabinit name prog.init otherTabs.init
    , update = tabupdate prog.update otherTabs.update
    , subscriptions = tabsubscriptions prog.subscriptions otherTabs.subscriptions
    }


tabinit : String -> ( model, Cmd msg ) -> ( Model a b, Cmd c ) -> ( Model model (Tab a b), Cmd (Message msg c) )
tabinit name ( headModel, headCmd ) ( tailModel, tailCmd ) =
    ( { tab = ( headModel, tailModel.tab ), selected = 0, names = name :: tailModel.names }
    , Cmd.batch
        [ Cmd.map MsgHead headCmd
        , Cmd.map MsgTail tailCmd
        ]
    )


tabview : (model -> Html.Html msg) -> (Model a b -> Html.Html c) -> Model model (Tab a b) -> Html.Html (Message msg c)
tabview view tailView model =
    let
        header i n =
            Html.li [ Events.onClick (Select i) ]
                [ Html.text n
                , Html.text "--"
                , Html.text
                    (if i == model.selected then
                        "*"
                     else
                        ""
                    )
                ]

        tabHeaders =
            Html.ul [] <| List.indexedMap header model.names
    in
        Html.div []
            [ tabHeaders
            , case model.selected of
                0 ->
                    App.map MsgHead <| view (fst model.tab)

                _ ->
                    App.map MsgTail <| tailView { names = [], selected = model.selected - 1, tab = snd model.tab }
            ]


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
