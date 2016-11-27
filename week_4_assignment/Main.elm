port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import String exposing (toInt)


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
    | Increment
    | SendCount
    | SetCount String


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

        Increment ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        SendCount ->
            ( model, sendCount model.count )

        SetCount strCount ->
            let
                count =
                    case toInt strCount of
                        Err _ ->
                            model.count

                        Ok newCount ->
                            newCount
            in
                ( { model | count = count }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "text"
            , value <| toString model.count
            , onInput SetCount
            ]
            []
        , button [ onClick SendCount ] [ text "Send Count" ]
        ]


port incrementCount : ({} -> msg) -> Sub msg


port sendCount : Int -> Cmd msg


mapIncrement : {} -> Msg
mapIncrement _ =
    Increment


subscriptions : Model -> Sub Msg
subscriptions model =
    incrementCount mapIncrement
