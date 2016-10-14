module Budget exposing (..)

import Html.App as App
import Html
import Html.Attributes
import Html.Events
import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=))
import DB
import Compose.Program as P
import String
import Date
import Date.Extra
import Time
import Viz.LineChart as LineChart
import D3.Axis as Axis


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
    | RenameCharge Int String
    | ChangeChargeAmount Int Float
    | ChangeChargeFreq Int Frequency
    | RemoveCharge Int


type Frequency
    = Once
    | Every Int Date.Extra.Interval


type alias Charge =
    { name : String, amount : Float, freq : Frequency, start : Date.Date }


type alias Model =
    { charges : List Charge }


type alias Balance =
    { date : Date.Date, balance : Float }


type alias Transaction =
    { date : Date.Date, amount : Float }


model : Model
model =
    { charges = [] }


view : Model -> Html.Html Msg
view model =
    let
        futureBalances =
            projectBalance model.charges

        chart =
            LineChart.chart (.date >> Date.toTime) .balance
                |> LineChart.xAxis (Axis.tickFormat (Date.fromTime >> Date.Extra.toFormattedString "MM-dd-YYYY"))
                |> LineChart.yAxis (Axis.tickFormat (Basics.round >> toString >> (\amount -> "$" ++ amount)))
                |> LineChart.width 800
    in
        Html.div []
            [ Html.h1 [] [ Html.text "Charges" ]
            , Html.button [ Html.Events.onClick AddCharge ] [ Html.text "Add Charge" ]
            , Html.ul [] (List.indexedMap viewCharge model.charges)
            , Html.hr [] []
            , LineChart.render chart futureBalances
            , Html.ul [] (List.map viewBalance futureBalances)
            ]


viewCharge : Int -> Charge -> Html.Html Msg
viewCharge i charge =
    let
        ( isOnce, isEvery ) =
            case charge.freq of
                Once ->
                    ( True, False )

                Every _ _ ->
                    ( False, True )

        stringToFreq s =
            case s of
                "once" ->
                    Once

                "every" ->
                    Every 1 Date.Extra.Month

                _ ->
                    Once

        stringToUnit s =
            case s of
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
    in
        Html.li []
            ([ Html.a [ Html.Attributes.href "#", Html.Events.onClick (RemoveCharge i) ] [ Html.text "Remove" ]
             , Html.text (toString charge.start)
             , Html.input
                [ Html.Attributes.type' "text"
                , Html.Attributes.value charge.name
                , Html.Events.onInput (RenameCharge i)
                ]
                []
             , Html.input
                [ Html.Attributes.type' "number"
                , Html.Attributes.value (toString charge.amount)
                , Html.Events.onInput (String.toFloat >> (Result.withDefault charge.amount) >> ChangeChargeAmount i)
                ]
                []
             , Html.select
                [ Html.Events.onInput (stringToFreq >> ChangeChargeFreq i)
                ]
                [ Html.option [ Html.Attributes.selected isOnce ] [ Html.text "once" ]
                , Html.option [ Html.Attributes.selected isEvery ] [ Html.text "every" ]
                ]
             ]
                ++ (case charge.freq of
                        Once ->
                            []

                        Every n unit ->
                            [ Html.input
                                [ Html.Attributes.type' "number"
                                , Html.Attributes.value (toString n)
                                , Html.Events.onInput (String.toInt >> (Result.withDefault n) >> (\n -> ChangeChargeFreq i (Every n unit)))
                                ]
                                []
                            , Html.select [ Html.Events.onInput (stringToUnit >> Every n >> ChangeChargeFreq i) ]
                                [ Html.option [ Html.Attributes.selected (unit == Date.Extra.Day) ] [ Html.text "day" ]
                                , Html.option [ Html.Attributes.selected (unit == Date.Extra.Week) ] [ Html.text "week" ]
                                , Html.option [ Html.Attributes.selected (unit == Date.Extra.Month) ] [ Html.text "month" ]
                                , Html.option [ Html.Attributes.selected (unit == Date.Extra.Year) ] [ Html.text "year" ]
                                ]
                            ]
                   )
            )


viewBalance : Balance -> Html.Html Msg
viewBalance b =
    Html.li [] [ Html.text (toString b) ]


projectBalance : List Charge -> List Balance
projectBalance charges =
    let
        endDate =
            Date.Extra.fromCalendarDate 2017 Date.Jan 1

        transactions =
            List.concatMap (projectCharge endDate) charges

        sortedTransactions =
            List.sortWith compareTransactions transactions

        addTransaction transaction balance =
            { date = transaction.date, balance = balance.balance + transaction.amount }

        balances =
            List.scanl addTransaction { date = Date.Extra.fromCalendarDate 2016 Date.Jan 1, balance = 0 } sortedTransactions
    in
        balances


compareTransactions : Transaction -> Transaction -> Order
compareTransactions a b =
    case Date.Extra.compare a.date b.date of
        EQ ->
            (case compare a.amount b.amount of
                GT ->
                    LT

                LT ->
                    GT

                EQ ->
                    EQ
            )

        other ->
            other


projectCharge : Date.Date -> Charge -> List Transaction
projectCharge until charge =
    if GT == Date.Extra.compare charge.start until then
        []
    else
        let
            newCharge =
                nextCharge charge
        in
            case newCharge of
                Nothing ->
                    [ { date = charge.start, amount = charge.amount } ]

                Just newCharge ->
                    { date = charge.start, amount = charge.amount } :: (projectCharge until newCharge)


nextCharge : Charge -> Maybe Charge
nextCharge c =
    case c.freq of
        Once ->
            Nothing

        Every n unit ->
            let
                newStart =
                    Date.Extra.add unit n c.start
            in
                Just { c | start = newStart }


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

        ChangeChargeAmount i value ->
            let
                charges =
                    replaceAt i model.charges (\c -> { c | amount = value })
            in
                ( { model | charges = charges }, Cmd.none )

        ChangeChargeFreq i freq ->
            let
                charges =
                    replaceAt i model.charges (\c -> { c | freq = freq })
            in
                ( { model | charges = charges }, Cmd.none )

        RemoveCharge i ->
            let
                charges =
                    removeAt i model.charges
            in
                ( { model | charges = charges }, Cmd.none )


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


deserialize : Decode.Decoder Model
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
    in
        Decode.object1 Model
            ("charges" := Decode.list charge)
