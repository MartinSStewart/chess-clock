module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Duration exposing (Duration)
import Time
import Url exposing (Url)


type Player
    = Player1
    | Player2


type FrontendModel
    = Setup SetupData
    | Ready ReadyData


type alias SetupData =
    { key : Key
    , time : Duration
    , increment : Int
    , vibrationEnabled : Bool
    }


type alias ReadyData =
    { key : Key
    , player1Time : Duration
    , player2Time : Duration
    , mode : Mode
    , lastTick : Time.Posix
    , increment : Duration
    , lastSwitchedAt : Time.Posix
    , vibrationEnabled : Bool
    }


type Mode
    = Paused
    | Running Player


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | SetupMsg SetupMsg
    | ReadyMsg ReadyMsg
    | GotSettingsFromLocalStorage LocalStorage


type alias LocalStorage =
    { vibrationEnabled : Bool, time : Int, increment : Int }


type SetupMsg
    = PressedPlusMinute
    | PressedMinusMinute
    | PressedPlusTenSeconds
    | PressedMinusTenSeconds
    | AdjustedIncrementSlider Int
    | ToggledVibration
    | PressedStart


type ReadyMsg
    = PlayerClicked Player
    | Tick Time.Posix
    | Pause
    | Reset


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
