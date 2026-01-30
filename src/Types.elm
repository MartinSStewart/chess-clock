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
    , mode : Mode
    , lastTick : Time.Posix -- last time we updated
    , increment : Int -- time increment in milliseconds
    , incrementInput : String -- text input for increment setting
    }


type Mode
    = Paused { editing : Maybe ( Player, String ) }
    | Running Player


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
