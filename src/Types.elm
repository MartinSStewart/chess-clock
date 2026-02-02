module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
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
    , time : Int
    , increment : Int
    }


type alias ReadyData =
    { key : Key
    , player1Time : Int -- milliseconds remaining
    , player2Time : Int -- milliseconds remaining
    , mode : Mode
    , lastTick : Time.Posix -- last time we updated
    , increment : Int -- time increment in milliseconds
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


type SetupMsg
    = PressedPlusMinute
    | PressedMinusMinute
    | PressedPlusTenSeconds
    | PressedMinusTenSeconds
    | AdjustedIncrementSlider Int
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
