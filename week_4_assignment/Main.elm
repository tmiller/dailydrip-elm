port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { count : Int }


type Msg
    = NoOp
    | Increment Bool


init : ( Model, Cmd Msg )
init =
    ( Model 0
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Increment _ ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "text"
            , value <| toString model.count
            ]
            []
        ]


port messages : (Bool -> msg) -> Sub msg


port increment : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    messages Increment
