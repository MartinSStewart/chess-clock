module Evergreen.V16.Types exposing (..)

import Browser
import Browser.Navigation
import Duration
import Time
import Url


type alias SetupData =
    { key : Browser.Navigation.Key
    , time : Duration.Duration
    , increment : Int
    , soundEnabled : Bool
    }


type Player
    = Player1
    | Player2


type Mode
    = Paused
    | Running Player


type alias ReadyData =
    { key : Browser.Navigation.Key
    , player1Time : Duration.Duration
    , player2Time : Duration.Duration
    , mode : Mode
    , lastTick : Time.Posix
    , increment : Duration.Duration
    , lastSwitchedAt : Time.Posix
    , totalElapsed : Duration.Duration
    , soundEnabled : Bool
    }


type FrontendModel
    = Setup SetupData
    | Ready ReadyData


type alias BackendModel =
    { message : String
    }


type SetupMsg
    = PressedPlusMinute
    | PressedMinusMinute
    | PressedPlusTenSeconds
    | PressedMinusTenSeconds
    | AdjustedIncrementSlider Int
    | ToggledSound
    | PressedStart


type ReadyMsg
    = PlayerClicked Player
    | Tick Time.Posix
    | Pause
    | Reset


type alias LocalStorage =
    { time : Int
    , increment : Int
    , soundEnabled : Bool
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SetupMsg SetupMsg
    | ReadyMsg ReadyMsg
    | GotSettingsFromLocalStorage LocalStorage


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
