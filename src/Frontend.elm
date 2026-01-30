module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Json
import Lamdera
import Time
import Types exposing (..)
import Url


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


defaultIncrement : Int
defaultIncrement =
    -- 5 seconds in milliseconds
    5 * 1000


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , player1Time = initialTime
      , player2Time = initialTime
      , mode = Paused { editing = Nothing }
      , lastTick = Time.millisToPosix 0
      , increment = defaultIncrement
      , incrementInput = "5"
      }
    , Cmd.none
    )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Time.every 100 Tick


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
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

        PlayerClicked player ->
            let
                ( mode, newPlayer1Time, newPlayer2Time ) =
                    case model.mode of
                        Paused paused ->
                            case paused.editing of
                                Just _ ->
                                    ( Paused { editing = Nothing }, model.player1Time, model.player2Time )

                                Nothing ->
                                    ( Running player, model.player1Time, model.player2Time )

                        Running Player1 ->
                            -- Player 1 finished turn, add increment to their time
                            ( Running Player2, model.player1Time + model.increment, model.player2Time )

                        Running Player2 ->
                            -- Player 2 finished turn, add increment to their time
                            ( Running Player1, model.player1Time, model.player2Time + model.increment )
            in
            ( { model
                | mode = mode
                , player1Time = newPlayer1Time
                , player2Time = newPlayer2Time
              }
            , Cmd.none
            )

        Pause ->
            ( { model | mode = Paused { editing = Nothing } }
            , Cmd.none
            )

        Reset ->
            ( { model
                | player1Time = initialTime
                , player2Time = initialTime
                , mode = Paused { editing = Nothing }
              }
            , Cmd.none
            )

        IncrementInputChanged value ->
            let
                newIncrement =
                    case String.toInt value of
                        Just seconds ->
                            seconds * 1000

                        Nothing ->
                            model.increment
            in
            ( { model | incrementInput = value, increment = newIncrement }
            , Cmd.none
            )

        TimeClicked player ->
            ( case model.mode of
                Paused paused ->
                    let
                        currentTime : Int
                        currentTime =
                            case player of
                                Player1 ->
                                    model.player1Time

                                Player2 ->
                                    model.player2Time
                    in
                    { model | mode = Paused { paused | editing = Just ( player, formatTimeForInput currentTime ) } }

                Running _ ->
                    model
            , Cmd.none
            )

        TimeInputChanged value ->
            ( { model
                | mode =
                    case model.mode of
                        Paused paused ->
                            { paused
                                | editing =
                                    case paused.editing of
                                        Just ( player, _ ) ->
                                            Just ( player, value )

                                        Nothing ->
                                            Nothing
                            }
                                |> Paused

                        Running _ ->
                            model.mode
              }
            , Cmd.none
            )

        TimeInputBlurred ->
            ( case model.mode of
                Paused paused ->
                    case paused.editing of
                        Just ( player, value ) ->
                            let
                                newTime =
                                    parseTimeInput value

                                ( newPlayer1Time, newPlayer2Time ) =
                                    case player of
                                        Player1 ->
                                            ( Maybe.withDefault model.player1Time newTime, model.player2Time )

                                        Player2 ->
                                            ( model.player1Time, Maybe.withDefault model.player2Time newTime )
                            in
                            { model
                                | player1Time = newPlayer1Time
                                , player2Time = newPlayer2Time
                            }

                        Nothing ->
                            model

                Running _ ->
                    model
            , Cmd.none
            )

        Tick currentTime ->
            let
                elapsed : Int
                elapsed =
                    if Time.posixToMillis model.lastTick == 0 then
                        0

                    else
                        Time.posixToMillis currentTime - Time.posixToMillis model.lastTick

                ( newPlayer1Time, newPlayer2Time ) =
                    case model.mode of
                        Running Player1 ->
                            ( max 0 (model.player1Time - elapsed), model.player2Time )

                        Running Player2 ->
                            ( model.player1Time, max 0 (model.player2Time - elapsed) )

                        Paused _ ->
                            ( model.player1Time, model.player2Time )
            in
            ( { model
                | player1Time = newPlayer1Time
                , player2Time = newPlayer2Time
                , lastTick = currentTime
                , mode =
                    if newPlayer1Time <= 0 || newPlayer2Time <= 0 then
                        Paused { editing = Nothing }

                    else
                        model.mode
              }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
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


formatTimeForInput : Int -> String
formatTimeForInput millis =
    let
        totalSeconds =
            millis // 1000

        minutes =
            totalSeconds // 60

        seconds =
            modBy 60 totalSeconds
    in
    String.fromInt minutes ++ ":" ++ String.padLeft 2 '0' (String.fromInt seconds)


parseTimeInput : String -> Maybe Int
parseTimeInput input =
    case String.split ":" input of
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
        [ Html.div
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
            [ viewTimer model Player1
            , viewTimer model Player2
            , viewControls model
            ]
        ]
    }


viewControls : FrontendModel -> Html FrontendMsg
viewControls model =
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "bottom" "20px"
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

            Paused _ ->
                [ Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "8px"
                    , Attr.style "background-color" "rgba(255,255,255,0.1)"
                    , Attr.style "padding" "10px 15px"
                    , Attr.style "border-radius" "8px"
                    ]
                    [ Html.span
                        [ Attr.style "color" "#fff"
                        , Attr.style "font-size" "14px"
                        ]
                        [ Html.text "Increment:" ]
                    , Html.input
                        [ Attr.type_ "number"
                        , Attr.value model.incrementInput
                        , Attr.style "width" "50px"
                        , Attr.style "padding" "8px"
                        , Attr.style "font-size" "16px"
                        , Attr.style "font-family" "monospace"
                        , Attr.style "border" "none"
                        , Attr.style "border-radius" "4px"
                        , Attr.style "text-align" "center"
                        , Events.onInput IncrementInputChanged
                        , Events.stopPropagationOn "click" (Json.succeed ( NoOpFrontendMsg, True ))
                        ]
                        []
                    , Html.span
                        [ Attr.style "color" "#fff"
                        , Attr.style "font-size" "14px"
                        ]
                        [ Html.text "sec" ]
                    ]
                , Html.button
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


viewTimer : FrontendModel -> Player -> Html FrontendMsg
viewTimer model player =
    let
        ( time, label, isActive ) =
            case player of
                Player1 ->
                    ( model.player1Time, "Player 1", model.mode == Running Player1 )

                Player2 ->
                    ( model.player2Time, "Player 2", model.mode == Running Player2 )

        backgroundColor =
            if time == 0 then
                "#ff4444"

            else if isActive then
                "#4CAF50"

            else
                "#333"

        textColor =
            "#fff"

        isPaused =
            case model.mode of
                Paused _ ->
                    True

                Running _ ->
                    False

        showTapToStart =
            isPaused && time > 0
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
        , Events.onClick (PlayerClicked player)
        ]
        [ Html.div
            [ Attr.style "font-size" "24px"
            , Attr.style "margin-bottom" "10px"
            , Attr.style "opacity" "0.8"
            ]
            [ Html.text label ]
        , case model.mode of
            Paused paused ->
                case paused.editing of
                    Just ( editingPlayer, value ) ->
                        if editingPlayer == player then
                            Html.input
                                [ Attr.type_ "text"
                                , Attr.value value
                                , Attr.style "font-size" "80px"
                                , Attr.style "font-weight" "bold"
                                , Attr.style "font-family" "monospace"
                                , Attr.style "width" "280px"
                                , Attr.style "text-align" "center"
                                , Attr.style "background-color" "rgba(255,255,255,0.2)"
                                , Attr.style "border" "2px solid #fff"
                                , Attr.style "border-radius" "8px"
                                , Attr.style "color" "#fff"
                                , Attr.style "padding" "10px"
                                , Events.onInput TimeInputChanged
                                , Events.onBlur TimeInputBlurred
                                , Events.stopPropagationOn "click" (Json.succeed ( NoOpFrontendMsg, True ))
                                ]
                                []

                        else
                            Html.div
                                [ Attr.style "font-size" "80px"
                                , Attr.style "font-weight" "bold"
                                , Attr.style "cursor" "text"
                                , Attr.style "padding" "10px 20px"
                                , Attr.style "border-radius" "8px"
                                , Attr.style "transition" "background-color 0.2s"
                                , Events.stopPropagationOn "click" (Json.succeed ( TimeClicked player, True ))
                                ]
                                [ Html.text (formatTime time) ]

                    Nothing ->
                        Html.div
                            [ Attr.style "font-size" "80px"
                            , Attr.style "font-weight" "bold"
                            , Attr.style "cursor" "text"
                            , Attr.style "padding" "10px 20px"
                            , Attr.style "border-radius" "8px"
                            , Attr.style "transition" "background-color 0.2s"
                            , Events.stopPropagationOn "click" (Json.succeed ( TimeClicked player, True ))
                            ]
                            [ Html.text (formatTime time) ]

            Running _ ->
                Html.div
                    [ Attr.style "font-size" "80px"
                    , Attr.style "font-weight" "bold"
                    ]
                    [ Html.text (formatTime time) ]
        , if showTapToStart then
            Html.div
                [ Attr.style "margin-top" "20px"
                , Attr.style "font-size" "16px"
                , Attr.style "opacity" "0.7"
                ]
                [ Html.text "Tap to start" ]
            --else if showTapToResume then
            --  Html.div
            --      [ Attr.style "margin-top" "20px"
            --      , Attr.style "font-size" "16px"
            --      , Attr.style "opacity" "0.7"
            --      ]
            --      [ Html.text "Tap to resume" ]
            --else if model.activePlayer == Nothing && time > 0 && not isEditingThis then
            --  Html.div
            --      [ Attr.style "margin-top" "20px"
            --      , Attr.style "font-size" "16px"
            --      , Attr.style "opacity" "0.7"
            --      ]
            --      [ Html.text "Click time to edit" ]

          else
            Html.text ""
        ]
