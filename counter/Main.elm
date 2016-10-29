module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as App


type alias Model =
    { count : Int
    , increment : Int
    , decrement : Int
    }


type Msg
    = Increment
    | Decrement


initialModel : Model
initialModel =
    { count = 0
    , increment = 0
    , decrement = 0
    }


increment : Model -> Model
increment model =
    { model | count = model.count + 1, increment = model.increment + 1 }


decrement : Model -> Model
decrement model =
    { model | count = model.count - 1, decrement = model.decrement + 1 }


update : Msg -> Model -> Model
update msg =
    case msg of
        Increment ->
            increment

        Decrement ->
            decrement


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text (toString model.count) ]
        , button [ onClick Decrement ] [ text "-" ]
        , h3 [] [ text ("+ clicked " ++ (toString model.increment) ++ " times") ]
        , h3 [] [ text ("- clicked " ++ (toString model.decrement) ++ " times") ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
