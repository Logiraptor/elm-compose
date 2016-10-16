module View exposing (..)

import Html
import Html.Events
import Html.Attributes
import Html.CssHelpers
import Date
import String
import Date.Extra
import D3.Axis as Axis
import Viz.LineChart as LineChart
import Model exposing (..)
import Style exposing (class, id)


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
            [ Html.header [ id Style.Header ]
                [ Html.h1 [] [ Html.text "HelpImBroke" ]
                , Html.h1 [ class [ Style.UserHeader ] ]
                    [ Html.text model.user.displayName
                    , Html.a [ Html.Events.onClick SignOut, Html.Attributes.href "#" ] [ Html.text "sign out" ]
                    ]
                ]
            , Html.button [ Html.Events.onClick AddCharge ] [ Html.text "Add Charge" ]
            , Html.ul [] (List.indexedMap viewCharge model.charges)
            , Html.hr [] []
            , LineChart.render chart futureBalances
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
            if n > 0 then
                let
                    newStart =
                        Date.Extra.add unit n c.start
                in
                    Just { c | start = newStart }
            else
                Nothing
