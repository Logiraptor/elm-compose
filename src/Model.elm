module Model exposing (..)

import Auth
import Date
import Date.Extra


type Msg
    = AddCharge
    | RenameCharge Int String
    | ChangeChargeAmount Int Float
    | ChangeChargeFreq Int Frequency
    | RemoveCharge Int
    | SignOut


type Frequency
    = Once
    | Every Int Date.Extra.Interval


type alias Charge =
    { name : String, amount : Float, freq : Frequency, start : Date.Date }


type alias Model =
    { charges : List Charge, user : Auth.User }


type alias Balance =
    { date : Date.Date, balance : Float }


type alias Transaction =
    { date : Date.Date, amount : Float }
