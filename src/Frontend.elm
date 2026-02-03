port module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Lamdera
import Quantity
import Time
import Types exposing (..)
import Url


port requestWakeLock : () -> Cmd msg


port releaseWakeLock : () -> Cmd msg


port vibrate : () -> Cmd msg


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


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( setupInit key
    , Cmd.none
    )


setupInit : Nav.Key -> FrontendModel
setupInit key =
    Setup
        { key = key
        , time = Duration.minutes 5
        , increment = 5
        }


readyInit : Nav.Key -> Duration -> Duration -> FrontendModel
readyInit key initialTime increment =
    { key = key
    , player1Time = initialTime
    , player2Time = initialTime
    , mode = Paused
    , lastTick = Time.millisToPosix 0
    , increment = increment
    , lastSwitchedAt = Time.millisToPosix 0
    }
        |> Ready


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    case model of
        Setup _ ->
            Sub.none

        Ready _ ->
            Time.every 100 (\time -> Tick time |> ReadyMsg)


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    (case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl
                        (case model of
                            Setup setup ->
                                setup.key

                            Ready ready ->
                                ready.key
                        )
                        (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        SetupMsg setupMsg ->
            case model of
                Setup setup ->
                    updateSetupMsg setupMsg setup

                Ready readyData ->
                    ( model, Cmd.none )

        ReadyMsg readyMsg ->
            case model of
                Setup setup ->
                    ( model, Cmd.none )

                Ready readyData ->
                    updateReadyMsg readyMsg readyData
    )
        |> (\( model2, cmd ) ->
                ( model2
                , Cmd.batch
                    [ if shouldEnableWakeLock model2 == shouldEnableWakeLock model then
                        Cmd.none

                      else if shouldEnableWakeLock model2 then
                        requestWakeLock ()

                      else
                        releaseWakeLock ()
                    , cmd
                    ]
                )
           )


updateSetupMsg : SetupMsg -> SetupData -> ( FrontendModel, Cmd FrontendMsg )
updateSetupMsg msg model =
    case msg of
        PressedPlusMinute ->
            ( Setup
                { model
                    | time =
                        if model.time |> Quantity.greaterThanOrEqualTo (Duration.minutes 10) then
                            Quantity.plus model.time (Duration.minutes 5)

                        else if model.time |> Quantity.greaterThanOrEqualTo (Duration.minutes 6) then
                            Quantity.plus model.time (Duration.minutes 2)

                        else
                            Quantity.plus model.time (Duration.minutes 1)
                }
            , Cmd.none
            )

        PressedMinusMinute ->
            ( Setup
                { model
                    | time =
                        (if model.time |> Quantity.greaterThanOrEqualTo (Duration.minutes 15) then
                            model.time |> Quantity.minus (Duration.minutes 5)

                         else if model.time |> Quantity.greaterThanOrEqualTo (Duration.minutes 8) then
                            model.time |> Quantity.minus (Duration.minutes 2)

                         else if model.time |> Quantity.greaterThanOrEqualTo (Duration.minutes 1) then
                            model.time |> Quantity.minus (Duration.minutes 1)

                         else
                            model.time
                        )
                            |> Quantity.max Quantity.zero
                }
            , Cmd.none
            )

        PressedPlusTenSeconds ->
            ( Setup { model | time = Quantity.plus model.time (Duration.seconds 10) }, Cmd.none )

        PressedMinusTenSeconds ->
            ( Setup { model | time = Quantity.max Quantity.zero (model.time |> Quantity.minus (Duration.seconds 10)) }, Cmd.none )

        AdjustedIncrementSlider value ->
            ( Setup { model | increment = value }, Cmd.none )

        PressedStart ->
            if model.time |> Quantity.greaterThan Quantity.zero then
                ( readyInit
                    model.key
                    model.time
                    (incrementSliderValueToIncrement model.increment |> toFloat |> Duration.seconds)
                , vibrate ()
                )

            else
                ( Setup model, Cmd.none )


updateReadyMsg : ReadyMsg -> ReadyData -> ( FrontendModel, Cmd FrontendMsg )
updateReadyMsg msg model =
    case msg of
        PlayerClicked player ->
            if Duration.from model.lastSwitchedAt model.lastTick |> Quantity.lessThan (Duration.seconds 0.4) then
                ( Ready model, Cmd.none )

            else
                let
                    ( mode, newPlayer1Time, newPlayer2Time ) =
                        case model.mode of
                            Paused ->
                                if
                                    (model.player1Time |> Quantity.greaterThan Quantity.zero)
                                        && (model.player2Time |> Quantity.greaterThan Quantity.zero)
                                then
                                    ( Running player, model.player1Time, model.player2Time )

                                else
                                    ( Paused, model.player1Time, model.player2Time )

                            Running Player1 ->
                                -- Player 1 finished turn, add increment to their time
                                ( Running Player2, Quantity.plus model.player1Time model.increment, model.player2Time )

                            Running Player2 ->
                                -- Player 2 finished turn, add increment to their time
                                ( Running Player1, model.player1Time, Quantity.plus model.player2Time model.increment )
                in
                ( { model
                    | mode = mode
                    , player1Time = newPlayer1Time
                    , player2Time = newPlayer2Time
                    , lastSwitchedAt = model.lastTick
                  }
                    |> Ready
                , vibrate ()
                )

        Pause ->
            ( { model | mode = Paused } |> Ready
            , Cmd.none
            )

        Reset ->
            ( setupInit model.key
            , Cmd.none
            )

        Tick currentTime ->
            let
                elapsed : Duration
                elapsed =
                    if Time.posixToMillis model.lastTick == 0 then
                        Quantity.zero

                    else
                        Duration.from model.lastTick currentTime

                ( newPlayer1Time, newPlayer2Time ) =
                    case model.mode of
                        Running Player1 ->
                            ( Quantity.max Quantity.zero (model.player1Time |> Quantity.minus elapsed), model.player2Time )

                        Running Player2 ->
                            ( model.player1Time, Quantity.max Quantity.zero (model.player2Time |> Quantity.minus elapsed) )

                        Paused ->
                            ( model.player1Time, model.player2Time )
            in
            ( { model
                | player1Time = newPlayer1Time
                , player2Time = newPlayer2Time
                , lastTick = currentTime
                , mode =
                    case ( (newPlayer1Time |> Quantity.lessThanOrEqualTo Quantity.zero) || (newPlayer2Time |> Quantity.lessThanOrEqualTo Quantity.zero), model.mode ) of
                        ( True, Running _ ) ->
                            Paused

                        _ ->
                            model.mode
              }
                |> Ready
            , Cmd.none
            )


shouldEnableWakeLock : FrontendModel -> Bool
shouldEnableWakeLock model =
    case model of
        Setup _ ->
            False

        Ready ready ->
            case ready.mode of
                Running _ ->
                    True

                Paused ->
                    False


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


formatTime : Duration -> String
formatTime duration =
    let
        totalSeconds =
            Duration.inSeconds duration |> round

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
    String.fromInt minutes ++ ":" ++ padZero seconds


parseTimeInput : String -> Maybe Int
parseTimeInput input =
    case String.split ":" input |> List.map String.trim of
        [ "", secStr ] ->
            Maybe.map (\s -> s * 1000) (String.toInt secStr)

        [ minStr, secStr ] ->
            Maybe.map2
                (\m s -> (m * 60 + s) * 1000)
                (String.toInt minStr)
                (String.toInt secStr)

        [ secStr ] ->
            Maybe.map (\s -> s * 1000) (String.toInt secStr)

        _ ->
            Nothing


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Chess Clock"
    , body =
        [ Html.node "style"
            []
            [ Html.text
                (String.join "\n"
                    [ "html, body {"
                    , "  margin: 0;"
                    , "  padding: 0;"
                    , "  background-color: " ++ inactiveColor ++ ";"
                    , "  overflow: hidden;"
                    , "  overscroll-behavior: none;"
                    , "  position: fixed;"
                    , "  width: 100%;"
                    , "  height: 100%;"
                    , "  touch-action: manipulation;"
                    , "}"
                    ]
                )
            ]
        , case model of
            Setup setup ->
                setupView setup |> Html.map SetupMsg

            Ready ready ->
                Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "flex-direction" "column"
                    , Attr.style "height" "100vh"
                    , Attr.style "width" "100vw"
                    , Attr.style "margin" "0"
                    , Attr.style "padding" "0"
                    , Attr.style "font-family" "monospace"
                    , Attr.style "user-select" "none"
                    , Attr.style "position" "relative"
                    ]
                    [ viewTimer ready Player1
                    , Html.div
                        [ Attr.style "height" "4px"
                        , Attr.style "background-color" "#5a5a5a"
                        , Attr.style "width" "100%"
                        ]
                        []
                    , viewTimer ready Player2
                    , viewControls ready
                    ]
                    |> Html.map ReadyMsg
        ]
    }


setupView : SetupData -> Html SetupMsg
setupView model =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "height" "100vh"
        , Attr.style "width" "100vw"
        , Attr.style "margin" "0"
        , Attr.style "padding" "0"
        , Attr.style "font-family" "monospace"
        , Attr.style "user-select" "none"
        , Attr.style "background-color" inactiveColor
        , Attr.style "color" "#fff"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "20px"
            ]
            [ Html.div
                [ Attr.style "font-size" "24px"
                , Attr.style "opacity" "0.8"
                ]
                [ Html.text "Set Time" ]
            , Html.div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "10px"
                ]
                [ Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "flex-direction" "column"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "8px"
                    ]
                    [ arrowButton "▲" PressedPlusMinute
                    , Html.div
                        [ Attr.style "font-size" "60px"
                        , Attr.style "font-weight" "bold"
                        , Attr.style "min-width" "100px"
                        , Attr.style "text-align" "center"
                        ]
                        [ String.fromInt (Duration.inMinutes model.time |> floor) |> String.padLeft 2 '0' |> Html.text ]
                    , arrowButton "▼" PressedMinusMinute
                    ]
                , Html.div
                    [ Attr.style "font-size" "60px"
                    , Attr.style "font-weight" "bold"
                    ]
                    [ Html.text ":" ]
                , Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "flex-direction" "column"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "8px"
                    ]
                    [ arrowButton "▲" PressedPlusTenSeconds
                    , Html.div
                        [ Attr.style "font-size" "60px"
                        , Attr.style "font-weight" "bold"
                        , Attr.style "min-width" "100px"
                        , Attr.style "text-align" "center"
                        ]
                        [ Html.text (String.padLeft 2 '0' (String.fromInt (Duration.inSeconds model.time |> floor |> modBy 60))) ]
                    , arrowButton "▼" PressedMinusTenSeconds
                    ]
                ]
            , Html.div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "10px"
                , Attr.style "margin-top" "20px"
                ]
                [ Html.div
                    [ Attr.style "font-size" "18px"
                    ]
                    [ Html.text ("Increment: " ++ String.fromInt (incrementSliderValueToIncrement model.increment) ++ "s") ]
                , Html.input
                    [ Attr.type_ "range"
                    , Attr.min "0"
                    , Attr.max "24"
                    , Attr.value (String.fromInt model.increment)
                    , Attr.style "width" "200px"
                    , Attr.style "cursor" "pointer"
                    , Events.onInput (\s -> AdjustedIncrementSlider (Maybe.withDefault 0 (String.toInt s)))
                    ]
                    []
                ]
            , Html.button
                [ Attr.style "padding" "20px 60px"
                , Attr.style "font-size" "24px"
                , Attr.style "background-color"
                    (if model.time |> Quantity.greaterThan Quantity.zero then
                        "#4CAF50"

                     else
                        "#666"
                    )
                , Attr.style "color" "#fff"
                , Attr.style "border" "none"
                , Attr.style "border-radius" "8px"
                , Attr.style "cursor"
                    (if model.time |> Quantity.greaterThan Quantity.zero then
                        "pointer"

                     else
                        "not-allowed"
                    )
                , Attr.style "font-family" "monospace"
                , Attr.style "margin-top" "30px"
                , Events.onClick PressedStart
                ]
                [ Html.text "Start" ]
            ]
        ]


incrementSliderValueToIncrement : Int -> Int
incrementSliderValueToIncrement value =
    if value <= 6 then
        value

    else
        case value - 6 of
            1 ->
                8

            2 ->
                10

            3 ->
                15

            4 ->
                20

            5 ->
                25

            6 ->
                30

            7 ->
                40

            8 ->
                50

            9 ->
                60

            10 ->
                80

            11 ->
                100

            12 ->
                120

            13 ->
                140

            14 ->
                160

            15 ->
                180

            16 ->
                200

            17 ->
                250

            _ ->
                300


arrowButton : String -> SetupMsg -> Html SetupMsg
arrowButton label msg =
    Html.button
        [ Attr.style "width" "60px"
        , Attr.style "height" "60px"
        , Attr.style "font-size" "30px"
        , Attr.style "background-color" "#333"
        , Attr.style "color" "#fff"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "8px"
        , Attr.style "cursor" "pointer"
        , Attr.style "font-family" "monospace"
        , Events.onClick msg
        ]
        [ Html.text label ]


viewControls : ReadyData -> Html ReadyMsg
viewControls model =
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "20px"
        , Attr.style "right" "20px"
        , Attr.style "display" "flex"
        , Attr.style "gap" "10px"
        , Attr.style "align-items" "center"
        ]
        (case model.mode of
            Running _ ->
                [ Html.button
                    [ Attr.style "padding" "15px 25px"
                    , Attr.style "font-size" "18px"
                    , Attr.style "background-color" "#ff9800"
                    , Attr.style "color" "#fff"
                    , Attr.style "border" "none"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "font-family" "monospace"
                    , Events.onClick Pause
                    ]
                    [ Html.text "Pause" ]
                ]

            Paused ->
                [ Html.button
                    [ Attr.style "padding" "15px 25px"
                    , Attr.style "font-size" "18px"
                    , Attr.style "background-color" "#2196F3"
                    , Attr.style "color" "#fff"
                    , Attr.style "border" "none"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "font-family" "monospace"
                    , Events.onClick Reset
                    ]
                    [ Html.text "Reset" ]
                ]
        )


inactiveColor : String
inactiveColor =
    "#1a1a1a"


viewTimer : ReadyData -> Player -> Html ReadyMsg
viewTimer model player =
    let
        ( time, label, isActive ) =
            case player of
                Player1 ->
                    ( model.player1Time, "Player 1", model.mode == Running Player1 )

                Player2 ->
                    ( model.player2Time, "Player 2", model.mode == Running Player2 )
    in
    Html.div
        [ Attr.style "flex" "1"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style
            "background-color"
            (if time |> Quantity.lessThanOrEqualTo Quantity.zero then
                "#ff4444"

             else if isActive && (time |> Quantity.lessThanOrEqualTo (Duration.seconds 30)) then
                "#ff9800"

             else if isActive then
                "#4CAF50"

             else
                inactiveColor
            )
        , Attr.style "color" "#fff"
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "background-color 0.2s"
        , Events.on "pointerdown" (Json.Decode.succeed (PlayerClicked player))
        ]
        ((if time |> Quantity.greaterThan Quantity.zero then
            Html.div
                [ Attr.style "font-size" "24px"
                , Attr.style "margin-bottom" "10px"
                , Attr.style "opacity" "0.8"
                ]
                [ Html.text label ]

          else
            Html.text ""
         )
            :: (case model.mode of
                    Paused ->
                        [ Html.div
                            [ Attr.style "font-size" "80px"
                            , Attr.style "font-weight" "bold"
                            , Attr.style "padding" "10px 20px"
                            , Attr.style "border-radius" "8px"
                            , Attr.style "transition" "background-color 0.2s"
                            ]
                            [ Html.text (formatTime time) ]
                        , if (model.player1Time |> Quantity.greaterThan Quantity.zero) && (model.player2Time |> Quantity.greaterThan Quantity.zero) then
                            Html.div
                                [ Attr.style "margin-top" "20px"
                                , Attr.style "font-size" "16px"
                                , Attr.style "opacity" "0.7"
                                ]
                                [ Html.text "Tap to start" ]

                          else
                            Html.text ""
                        ]

                    Running _ ->
                        [ Html.div
                            [ Attr.style "font-size" "80px"
                            , Attr.style "font-weight" "bold"
                            ]
                            [ Html.text (formatTime time) ]
                        ]
               )
        )
