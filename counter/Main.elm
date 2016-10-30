module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as App


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text (toString model) ]
        , button [ onClick Decrement ] [ text "-" ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = 0
        , view = view
        , update = update
        }
