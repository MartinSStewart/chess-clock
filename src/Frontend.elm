module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Lamdera
import Time
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


initialTime : Int
initialTime =
    -- 5 minutes in milliseconds
    5 * 60 * 1000


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , player1Time = initialTime
      , player2Time = initialTime
      , activePlayer = Nothing
      , lastTick = Time.millisToPosix 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    case model.activePlayer of
        Just _ ->
            Time.every 100 Tick

        Nothing ->
            Sub.none


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        SwitchPlayer ->
            let
                newActivePlayer =
                    case model.activePlayer of
                        Nothing ->
                            Just Player1

                        Just Player1 ->
                            Just Player2

                        Just Player2 ->
                            Just Player1
            in
            ( { model | activePlayer = newActivePlayer }
            , Cmd.none
            )

        Tick currentTime ->
            let
                elapsed =
                    if Time.posixToMillis model.lastTick == 0 then
                        0

                    else
                        Time.posixToMillis currentTime - Time.posixToMillis model.lastTick

                ( newPlayer1Time, newPlayer2Time ) =
                    case model.activePlayer of
                        Just Player1 ->
                            ( max 0 (model.player1Time - elapsed), model.player2Time )

                        Just Player2 ->
                            ( model.player1Time, max 0 (model.player2Time - elapsed) )

                        Nothing ->
                            ( model.player1Time, model.player2Time )

                -- Stop the clock if time runs out
                newActivePlayer =
                    if newPlayer1Time == 0 || newPlayer2Time == 0 then
                        Nothing

                    else
                        model.activePlayer
            in
            ( { model
                | player1Time = newPlayer1Time
                , player2Time = newPlayer2Time
                , lastTick = currentTime
                , activePlayer = newActivePlayer
              }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


formatTime : Int -> String
formatTime millis =
    let
        totalSeconds =
            millis // 1000

        minutes =
            totalSeconds // 60

        seconds =
            modBy 60 totalSeconds

        padZero n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n
    in
    padZero minutes ++ ":" ++ padZero seconds


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Chess Clock"
    , body =
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "height" "100vh"
            , Attr.style "width" "100vw"
            , Attr.style "margin" "0"
            , Attr.style "padding" "0"
            , Attr.style "font-family" "monospace"
            , Attr.style "user-select" "none"
            , Events.onClick SwitchPlayer
            ]
            [ viewTimer model Player1
            , viewTimer model Player2
            ]
        ]
    }


viewTimer : Model -> Player -> Html FrontendMsg
viewTimer model player =
    let
        ( time, label, isActive ) =
            case player of
                Player1 ->
                    ( model.player1Time, "Player 1", model.activePlayer == Just Player1 )

                Player2 ->
                    ( model.player2Time, "Player 2", model.activePlayer == Just Player2 )

        backgroundColor =
            if time == 0 then
                "#ff4444"

            else if isActive then
                "#4CAF50"

            else
                "#333"

        textColor =
            "#fff"
    in
    Html.div
        [ Attr.style "flex" "1"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "background-color" backgroundColor
        , Attr.style "color" textColor
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "background-color 0.2s"
        ]
        [ Html.div
            [ Attr.style "font-size" "24px"
            , Attr.style "margin-bottom" "10px"
            , Attr.style "opacity" "0.8"
            ]
            [ Html.text label ]
        , Html.div
            [ Attr.style "font-size" "80px"
            , Attr.style "font-weight" "bold"
            ]
            [ Html.text (formatTime time) ]
        , if model.activePlayer == Nothing && time > 0 then
            Html.div
                [ Attr.style "margin-top" "20px"
                , Attr.style "font-size" "16px"
                , Attr.style "opacity" "0.7"
                ]
                [ Html.text "Tap to start" ]

          else
            Html.text ""
        ]
