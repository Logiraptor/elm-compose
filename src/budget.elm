port module Budget exposing (..)

import Html.App as App
import Html
import Html.Events


port put : SyncedModel -> Cmd msg


port updates : (SyncedModel -> msg) -> Sub msg


main : Program Never
main =
    App.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Commit SyncedModel
    | AddCharge


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
    { syncState : Maybe Bool, charges : List Charge }


model : Model
model =
    { syncState = Nothing, charges = [] }


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
        Commit m ->
            let
                new =
                    deserialize m
            in
                ( { new | syncState = Just True }, Cmd.none )

        AddCharge ->
            let
                newCharge =
                    { name = "NewCharge", amount = 0.0, freq = Once }
            in
                ( model, put (serialize { model | charges = newCharge :: model.charges }) )


subscriptions : Model -> Sub Msg
subscriptions model =
    updates Commit


type alias SyncedCharge =
    { name : String, amount : Float, freq : ( String, Int, String ) }


type alias SyncedModel =
    { charges : List SyncedCharge }


serialize : Model -> SyncedModel
serialize model =
    let
        unit u =
            case u of
                Day ->
                    "day"

                Week ->
                    "week"

                Month ->
                    "month"

                Year ->
                    "year"

        freq f =
            case f of
                Once ->
                    ( "once", 0, "" )

                Every n units ->
                    ( "every", n, (unit units) )

        charge c =
            { name = c.name, amount = c.amount, freq = freq c.freq }
    in
        { charges = List.map charge model.charges }


deserialize : SyncedModel -> Model
deserialize model =
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

        freq ( f, n, u ) =
            case ( f, n, u ) of
                ( "once", 0, "" ) ->
                    Once

                ( "every", n, u ) ->
                    Every n (unit u)

                _ ->
                    Once

        charge c =
            { name = c.name, amount = c.amount, freq = freq c.freq }
    in
        { charges = List.map charge model.charges, syncState = Nothing }
