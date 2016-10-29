module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as App


type alias Model =
    { counter : Int
    , inc : Int
    , dec : Int
    }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model
                | counter = model.counter + 1
                , inc = model.inc + 1
            }

        Decrement ->
            { model
                | counter = model.counter - 1
                , dec = model.dec + 1
            }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text (toString model.counter) ]
        , button [ onClick Decrement ] [ text "-" ]
        , h3 [] [ text ("Inc: " ++ toString model.inc) ]
        , h3 [] [ text ("Dec: " ++ toString model.dec) ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = { inc = 0, dec = 0, counter = 0 }
        , view = view
        , update = update
        }
