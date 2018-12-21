module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import RemoteData exposing (..)


weatherUrl : String
weatherUrl =
    "http://api.openweathermap.org/data/2.5/weather?appid=" ++ apiKey ++ "&q="


apiKey : String
apiKey =
    "b6595c7489fa224f03df3575d4d04389"



{--- MODEL ----}


type alias WeatherReport =
    { place : String
    , weather : Maybe String
    }


type alias Model =
    { weatherReports : List (WebData WeatherReport)
    , searchTerm : Maybe String
    , error : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { weatherReports = []
      , searchTerm = Nothing
      , error = False
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateSearchTerm String
    | RequestWeatherReport
    | ReceivedWeatherReport (WebData WeatherReport)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateSearchTerm s ->
            ( { model | searchTerm = Just s }
            , Cmd.none
            )

        RequestWeatherReport ->
            model.searchTerm
                |> Maybe.map (\s -> ( model, getWeather s ))
                |> Maybe.withDefault ( model, Cmd.none )

        ReceivedWeatherReport response ->
            ( { model
                | searchTerm = Nothing
                , error = False
                , weatherReports = response :: model.weatherReports
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Enter a city to see the weather" ]
        , Html.form [ onSubmit RequestWeatherReport ]
            [ input
                [ autofocus True
                , type_ "text"
                , onInput UpdateSearchTerm
                , value
                    (Maybe.withDefault ""
                        model.searchTerm
                    )
                ]
                []
            ]
        , if model.error then
            div [ class "error" ] [ text "Oh no something went wrong" ]

          else
            text ""
        , div [] (weatherReports model.weatherReports)
        ]


weatherReports : List (WebData WeatherReport) -> List (Html Msg)
weatherReports =
    List.map
        (\r ->
            div
                [ class "weather-report" ]
                [ case r of
                    NotAsked ->
                        text "We haven't asked for the data yet"

                    Loading ->
                        text "Loading ..."

                    Failure err ->
                        div [ class "error" ] [ text ("Error: " ++ toString err) ]

                    Success report ->
                        text
                            (report.place
                                ++ ": "
                                ++ Maybe.withDefault "Unknown" report.weather
                            )
                ]
        )



---- API ------


getWeather : String -> Cmd Msg
getWeather s =
    Http.send (RemoteData.fromResult >> ReceivedWeatherReport) <|
        Http.get (weatherUrl ++ s) weatherReportDecoder


weatherReportDecoder : Decode.Decoder WeatherReport
weatherReportDecoder =
    Decode.map2 WeatherReport
        (Decode.field "name" Decode.string)
        (Decode.map List.head
            (Decode.field "weather"
                (Decode.list
                    (Decode.field "description"
                        Decode.string
                    )
                )
            )
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
