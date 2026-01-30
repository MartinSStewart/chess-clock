module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Time
import Url


type Player
    = Player1
    | Player2


type Mode
    = Paused
        { editing : Maybe ( Player, String )
        }
    | Running Player


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , player1Time : Int
    , player2Time : Int
    , mode : Mode
    , lastTick : Time.Posix
    , increment : Int
    , incrementInput : String
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PlayerClicked Player
    | Tick Time.Posix
    | Pause
    | Reset
    | IncrementInputChanged String
    | TimeClicked Player
    | TimeInputChanged String
    | TimeInputBlurred


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
