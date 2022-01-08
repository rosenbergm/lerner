module Main exposing (..)

import Array
import Browser
import Csv.Decode as CSVD
import Html exposing (Html, button, div, footer, h1, input, section, text)
import Html.Events exposing (onClick, onInput)
import Http
import Random exposing (generate)
import Random.List exposing (shuffle)


main : Program {} Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = always Sub.none }


type Step
    = SelectSource
    | Cards


type alias Model =
    { step : Step
    , inputSheetId : Maybe String
    , data : Maybe (Array.Array ( String, String ))
    , currentCard : Int
    , cardFlipped : Bool
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { step = SelectSource
      , inputSheetId = Nothing
      , data = Nothing
      , currentCard = 0
      , cardFlipped = False
      }
    , Cmd.none
    )


tableDataDecoder : CSVD.Decoder ( String, String )
tableDataDecoder =
    CSVD.map2 (\first second -> ( first, second ))
        (CSVD.column 0 CSVD.string)
        (CSVD.column 1 CSVD.string)


getTableData : String -> Cmd Msg
getTableData sheetId =
    Http.get
        { url = "https://docs.google.com/spreadsheets/d/" ++ sheetId ++ "/export?format=csv"
        , expect = Http.expectString GotTableData
        }


type Msg
    = FetchDocument
    | ChangeStep Step
    | ChangeSheetId String
    | GotTableData (Result Http.Error String)
    | ProceedWithSheetId
    | ProcessShuffledCards (List ( String, String ))
    | PreviousCard
    | NextCard
    | FlipCard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDocument ->
            ( model, Cmd.none )

        ChangeStep step ->
            ( { model | step = step }, Cmd.none )

        ChangeSheetId sheetId ->
            ( { model | inputSheetId = Just sheetId }, Cmd.none )

        ProceedWithSheetId ->
            case model.inputSheetId of
                Nothing ->
                    ( model, Cmd.none )

                Just sheetId ->
                    ( model, getTableData sheetId )

        GotTableData data ->
            case data of
                Ok csvString ->
                    let
                        decodedCsv =
                            CSVD.decodeCsv CSVD.NoFieldNames tableDataDecoder csvString
                    in
                    case decodedCsv of
                        Ok csv ->
                            ( model, generate ProcessShuffledCards (shuffle csv) )

                        Err _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ProcessShuffledCards shuffled ->
            ( { model | data = Just <| Array.fromList shuffled, step = Cards }, Cmd.none )

        FlipCard ->
            ( { model | cardFlipped = not model.cardFlipped }, Cmd.none )

        NextCard ->
            let
                dataLength =
                    Array.length (Maybe.withDefault Array.empty model.data)
            in
            if model.currentCard == dataLength - 1 then
                ( { model | currentCard = 0 }, Cmd.none )

            else
                ( { model | currentCard = model.currentCard + 1 }, Cmd.none )

        PreviousCard ->
            if model.currentCard == 0 then
                let
                    dataLength =
                        Array.length (Maybe.withDefault Array.empty model.data)
                in
                ( { model | currentCard = dataLength - 1 }, Cmd.none )

            else
                ( { model | currentCard = model.currentCard - 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.step of
        SelectSource ->
            div []
                [ text "enter link to google form"
                , input [ onInput ChangeSheetId ] []
                , button [ onClick ProceedWithSheetId ] [ text "Go!" ]
                ]

        Cards ->
            case model.data of
                Nothing ->
                    text "There is no data ):. Refresh the page and start again. The sheet must be a public one."

                Just data ->
                    section [] <|
                        [ div []
                            [ h1 []
                                [ text <|
                                    case Array.get model.currentCard data of
                                        Nothing ->
                                            "oops, no-mans land"

                                        Just ( a, b ) ->
                                            let
                                                term =
                                                    if model.cardFlipped then
                                                        b

                                                    else
                                                        a

                                                stringWithDefault string =
                                                    if String.isEmpty string then
                                                        "no term supplied"

                                                    else
                                                        string
                                            in
                                            stringWithDefault term
                                ]
                            ]
                        , footer []
                            [ button [ onClick PreviousCard ] [ text "previous" ]
                            , button [ onClick FlipCard ] [ text "flip it" ]
                            , text <| "Card " ++ String.fromInt (model.currentCard + 1) ++ " of " ++ (String.fromInt <| Array.length <| data)
                            , button [ onClick NextCard ] [ text "next" ]
                            ]
                        ]
