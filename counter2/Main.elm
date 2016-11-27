port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int
    , increment : Int
    , decrement : Int
    }


type Msg
    = Increment
    | Decrement
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( { count = 0
      , increment = 0
      , decrement = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1, increment = model.increment + 1 }
            , increment ()
            )

        Decrement ->
            ( { model | count = model.count - 1, decrement = model.decrement + 1 }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text (toString model.count) ]
        , button [ onClick Decrement ] [ text "-" ]
        , h3 [] [ text ("+ clicked " ++ (toString model.increment) ++ " times") ]
        , h3 [] [ text ("- clicked " ++ (toString model.decrement) ++ " times") ]
        ]


port jsMsgs : (Int -> msg) -> Sub msg


port increment : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    jsMsgs mapJsMsg


mapJsMsg : Int -> Msg
mapJsMsg int =
    case int of
        1 ->
            Increment

        _ ->
            NoOp


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
