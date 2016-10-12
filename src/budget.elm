module Budget exposing (..)

import Html.App as App
import Html
import Html.Events
import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=))
import DB
import Compose.Program as P


main : Program Never
main =
    App.program <| DB.persistent serialize decoder app


decoder : Encode.Value -> Model
decoder =
    Decode.decodeValue deserialize >> Result.withDefault model


app : P.Program Model Msg
app =
    { init = ( model, Cmd.none )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type Msg
    = AddCharge


type Frequency
    = Once
    | Every Int TimeUnit


type TimeUnit
    = Day
    | Week
    | Month
    | Year


type alias Charge =
    { name : String, amount : Float, freq : Frequency }


type alias Model =
    { charges : List Charge }


model : Model
model =
    { charges = [] }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Charges" ]
        , Html.ul [] (List.map viewCharge model.charges)
        , Html.button [ Html.Events.onClick AddCharge ] [ Html.text "+" ]
        ]


viewCharge : Charge -> Html.Html Msg
viewCharge charge =
    Html.li []
        [ Html.text charge.name
        , Html.text " "
        , Html.b [] [ Html.text (toString charge.amount) ]
        , Html.text " "
        , Html.i [] [ Html.text (toString charge.freq) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddCharge ->
            let
                newCharge =
                    { name = "NewCharge", amount = 0.0, freq = Once }
            in
                ( { model | charges = newCharge :: model.charges }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


serialize : Model -> Encode.Value
serialize model =
    let
        unit u =
            case u of
                Day ->
                    Encode.string "day"

                Week ->
                    Encode.string "week"

                Month ->
                    Encode.string "month"

                Year ->
                    Encode.string "year"

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
                ]
    in
        Encode.object [ ( "charges", Encode.list <| List.map charge model.charges ) ]


deserialize : Decode.Decoder Model
deserialize =
    let
        unit u =
            case u of
                "day" ->
                    Day

                "week" ->
                    Week

                "month" ->
                    Month

                "year" ->
                    Year

                _ ->
                    Month

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
            Decode.object3 Charge
                ("name" := Decode.string)
                ("amount" := Decode.float)
                ("freq" := Decode.tuple3 freq Decode.string Decode.int Decode.string)
    in
        Decode.object1 Model
            ("charges" := Decode.list charge)
