module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Time
import Url exposing (Url)


type Player
    = Player1
    | Player2


type alias FrontendModel =
    { key : Key
    , player1Time : Int -- milliseconds remaining
    , player2Time : Int -- milliseconds remaining
    , activePlayer : Maybe Player -- which timer is running, Nothing if paused/stopped
    , lastTick : Time.Posix -- last time we updated
    , isPaused : Bool -- True if game was started but is now paused
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PlayerClicked Player
    | Tick Time.Posix
    | Pause
    | Reset


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
