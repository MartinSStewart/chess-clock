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
    , activePlayer : Maybe Player -- which timer is running, Nothing if not started
    , lastTick : Time.Posix -- last time we updated
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | SwitchPlayer
    | Tick Time.Posix


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
