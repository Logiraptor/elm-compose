module Main exposing (..)

import Html exposing (Html, button, div, text, p, input, pre)
import Html.App as App
import Html.Attributes exposing (style, name)
import String
import Sexp exposing (..)
import Compose.Program as P
import Compose.Collapse as Collapse
import Compose.Tabs as Tabs


main : Program Never
main =
    P.beginnerProgram { model = model, view = view, update = update }
        |> Collapse.collapse
        |> Tabs.tabbed "main"
        |> Tabs.with "other" (P.beginnerProgram { model = model, view = view, update = update })
        |> Tabs.with "other2" (P.beginnerProgram { model = model, view = view, update = update })
        |> Tabs.with "other3" (P.beginnerProgram { model = model, view = view, update = update })
        |> App.program



-- MODEL


type Widget
    = Text String
    | Input String
    | Button String
    | Columns (List Widget)
    | Rows (List Widget)


type alias Model =
    { widget : Widget
    }


model : Model
model =
    { widget =
        Columns
            [ Rows
                [ Text "Username:"
                , Input "username"
                , Text "Password"
                , Input "password"
                , Button "Submit"
                ]
            , Rows
                [ Text "Username:"
                , Input "username"
                , Text "Password"
                , Input "password"
                , Button "Submit"
                ]
            ]
    }


toSexp : Widget -> Sexp
toSexp widget =
    case widget of
        Text s ->
            Str s

        Input n ->
            Group [ Atom "input", Str n ]

        Button n ->
            Group [ Atom "button", Str n ]

        Columns cols ->
            Group ([ Atom "columns" ] ++ List.map toSexp cols)

        Rows rows ->
            Group ([ Atom "rows" ] ++ List.map toSexp rows)



-- UPDATE


type Msg
    = Nothing


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style [ ( "width", "50%" ), ( "height", "100%" ) ] ] [ viewWidget model.widget ]
        , pre [] [ text (sexpToString <| toSexp model.widget) ]
        ]


viewWidget : Widget -> Html Msg
viewWidget widget =
    let
        commonStyle =
            [ ( "flex", "1" ) ]
    in
        case widget of
            Text s ->
                div [ style commonStyle ] [ text s ]

            Input n ->
                div [ style commonStyle ] [ input [ name n ] [] ]

            Button s ->
                div [ style commonStyle ] [ button [] [ text s ] ]

            Rows rows ->
                div [ style ([ ( "display", "flex" ), ( "flex-direction", "column" ) ] ++ commonStyle) ] <| List.map viewWidget rows

            Columns cols ->
                div [ style ([ ( "display", "flex" ), ( "flex-direction", "row" ) ] ++ commonStyle) ] <| List.map viewWidget cols


toPixels : a -> String
toPixels value =
    toString value ++ "px"
