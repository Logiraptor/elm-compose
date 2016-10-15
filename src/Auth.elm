port module Auth exposing (authenticated, Msg(ChangeState, SignIn, SignOut), Provider, User, signOut, signIn)

import Compose.Program as P
import Html
import Html.App as App


type alias Model authed unauthed =
    { state : Bool, authed : authed, unauthed : unauthed }


type Msg authed unauthed
    = Authenticated authed
    | Unauthenticated unauthed
    | ChangeState (Maybe User)
    | SignIn Provider
    | SignOut


type alias User =
    { uid : String }


type alias Provider =
    String


port authValues : (Maybe User -> msg) -> Sub msg


port signIn : Provider -> Cmd msg


port signOut : () -> Cmd msg


authenticated : P.Program a b -> P.Program c d -> P.Program (Model a c) (Msg b d)
authenticated authedProg unauthedProg =
    { view = authenticatedView authedProg.view unauthedProg.view
    , update = authenticatedUpdate authedProg.update unauthedProg.update
    , subscriptions = authenticatedSubscriptions authedProg.subscriptions unauthedProg.subscriptions
    , init = authenticatedInit authedProg.init unauthedProg.init
    }


authenticatedView : (a -> Html.Html b) -> (c -> Html.Html d) -> Model a c -> Html.Html (Msg b d)
authenticatedView authed unauthed model =
    case model.state of
        True ->
            App.map Authenticated (authed model.authed)

        False ->
            App.map Unauthenticated (unauthed model.unauthed)


authenticatedUpdate : (a -> b -> ( b, Cmd a )) -> (e -> f -> ( f, Cmd e )) -> Msg a e -> Model b f -> ( Model b f, Cmd (Msg a e) )
authenticatedUpdate authed unauthed msg model =
    case msg of
        Authenticated a ->
            let
                ( m, cmds ) =
                    authed a model.authed
            in
                ( { model | authed = m }, Cmd.map Authenticated cmds )

        Unauthenticated a ->
            let
                ( m, cmds ) =
                    unauthed a model.unauthed
            in
                ( { model | unauthed = m }, Cmd.map Unauthenticated cmds )

        SignIn s ->
            ( model, signIn s )

        SignOut ->
            ( model, signOut () )

        ChangeState user ->
            case user of
                Nothing ->
                    ( { model | state = False }, Cmd.none )

                Just u ->
                    ( { model | state = True }, Cmd.none )


authenticatedSubscriptions : (a -> Sub b) -> (c -> Sub d) -> Model a c -> Sub (Msg b d)
authenticatedSubscriptions authed unauthed model =
    case model.state of
        True ->
            Sub.batch [ Sub.map Authenticated (authed model.authed), authValues ChangeState ]

        False ->
            Sub.batch [ Sub.map Unauthenticated (unauthed model.unauthed), authValues ChangeState ]


authenticatedInit : ( a, Cmd b ) -> ( c, Cmd d ) -> ( Model a c, Cmd (Msg b d) )
authenticatedInit ( authed, authedCmds ) ( unauthed, unauthedCmds ) =
    ( { state = False, authed = authed, unauthed = unauthed }, Cmd.batch [ Cmd.map Authenticated authedCmds, Cmd.map Unauthenticated unauthedCmds ] )
