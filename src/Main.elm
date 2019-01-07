module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



---- MODEL ----


url : String
url =
    "http://api.openweathermap.org/data/2.5/weather?appid=" ++ apiKey ++ "&q="


apiKey : String
apiKey =
    "b6595c7489fa224f03df3575d4d04389"


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "ef-section -s u-bg-education-paper" ]
        [ div [ class "ef-container" ]
            [ h3 [ class "ef-h3" ] [ text "Enter a city to see the weather" ]
            , Html.form []
                [ div
                    [ class "ef-input-w u-mb-m" ]
                    [ input
                        [ class "ef-input"
                        , placeholder "Search for the weather in a city"
                        , autofocus True
                        , type_ "text"
                        ]
                        []
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
