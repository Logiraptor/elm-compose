module Budget exposing (..)

import Html.App as App
import Html
import Html.Events
import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=))
import DB
import Auth
import Compose.Program as P
import Date
import Date.Extra
import Html.CssHelpers
import Model exposing (..)
import View


main : Program Never
main =
    let
        authed =
            DB.persistent serialize decoder app

        unauthed =
            nothingApp
    in
        Auth.authenticated authed unauthed
            |> App.program


decoder : Model -> Encode.Value -> Model
decoder model value =
    let
        { charges } =
            Decode.decodeValue deserialize value |> Result.withDefault { charges = [] }
    in
        { model | charges = charges }


app : P.Program Model Msg
app =
    { init = ( model, Cmd.none )
    , view = View.view
    , update = update
    , subscriptions = subscriptions
    }


nothingApp : P.Program Int Int
nothingApp =
    { init = ( 0, Cmd.none )
    , view = (\i -> Html.i [ Html.Events.onClick 0 ] [ Html.text "Hello" ])
    , update =
        (\c m ->
            case c of
                0 ->
                    ( 0, Auth.signIn "Google" )

                _ ->
                    ( 0, Cmd.none )
        )
    , subscriptions =
        (\m -> Sub.none)
    }


model : Model
model =
    { charges = [], user = Auth.anonymous }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCharge ->
            let
                newCharge =
                    { name = "NewCharge", amount = 0.0, freq = Once, start = Date.Extra.fromParts 2016 Date.Jan 1 0 0 0 0 }
            in
                ( { model | charges = model.charges ++ [ newCharge ] }, Cmd.none )

        RenameCharge i newName ->
            let
                charges =
                    replaceAt i model.charges (\c -> { c | name = newName })
            in
                ( { model | charges = charges }, Cmd.none )

        ChangeChargeAmount i amount ->
            let
                charges =
                    replaceAt i model.charges (\c -> { c | amount = amount })
            in
                ( { model | charges = charges }, Cmd.none )

        ChangeChargeFreq i freq ->
            let
                charges =
                    replaceAt i model.charges (\c -> { c | freq = freq })
            in
                ( { model | charges = charges }, Cmd.none )

        ChangeChargeStart i start ->
            let
                charges =
                    replaceAt i model.charges (\c -> { c | start = start })
            in
                ( { model | charges = charges }, Cmd.none )

        RemoveCharge i ->
            let
                charges =
                    removeAt i model.charges
            in
                ( { model | charges = charges }, Cmd.none )

        SignOut ->
            ( model, Auth.signOut () )


removeAt : Int -> List a -> List a
removeAt i l =
    List.take i l ++ List.drop (i + 1) l


replaceAt : Int -> List a -> (a -> a) -> List a
replaceAt index list func =
    let
        elem =
            List.head <| List.drop index list
    in
        case elem of
            Nothing ->
                list

            Just elem ->
                List.take index list ++ [ func elem ] ++ List.drop (index + 1) list


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


serialize : Model -> Encode.Value
serialize model =
    let
        unit u =
            case u of
                Date.Extra.Day ->
                    Encode.string "day"

                Date.Extra.Week ->
                    Encode.string "week"

                Date.Extra.Month ->
                    Encode.string "month"

                Date.Extra.Year ->
                    Encode.string "year"

                _ ->
                    Encode.string "month"

        freq f =
            case f of
                Once ->
                    Encode.list [ Encode.string "once", Encode.int 0, Encode.string "" ]

                Every n units ->
                    Encode.list [ Encode.string "every", Encode.int n, (unit units) ]

        charge c =
            Encode.object
                [ ( "name", Encode.string c.name )
                , ( "amount", Encode.float c.amount )
                , ( "freq", freq c.freq )
                , ( "start", Encode.string (Date.Extra.toIsoString c.start) )
                ]
    in
        Encode.object [ ( "charges", Encode.list <| List.map charge model.charges ) ]


deserialize : Decode.Decoder { charges : List Charge }
deserialize =
    let
        unit u =
            case u of
                "day" ->
                    Date.Extra.Day

                "week" ->
                    Date.Extra.Week

                "month" ->
                    Date.Extra.Month

                "year" ->
                    Date.Extra.Year

                _ ->
                    Date.Extra.Month

        freq : String -> Int -> String -> Frequency
        freq f n u =
            case ( f, n, u ) of
                ( "once", 0, "" ) ->
                    Once

                ( "every", n, u ) ->
                    Every n (unit u)

                _ ->
                    Once

        charge : Decode.Decoder Charge
        charge =
            Decode.object4 Charge
                ("name" := Decode.string)
                ("amount" := Decode.float)
                ("freq" := Decode.tuple3 freq Decode.string Decode.int Decode.string)
                ("start" := Decode.object1 (Date.Extra.fromIsoString >> Maybe.withDefault (Date.Extra.fromCalendarDate 2016 Date.Jan 1)) Decode.string)

        output charges =
            { charges = charges }
    in
        Decode.object1 output
            ("charges" := Decode.list charge)
