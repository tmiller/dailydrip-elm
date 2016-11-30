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
    | Set Model
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
            let
                newModel =
                    { model
                        | count = model.count + 1
                        , increment = model.increment + 1
                    }
            in
                ( newModel
                , Cmd.batch [ increment (), storage newModel ]
                )

        Decrement ->
            let
                newModel =
                    { model
                        | count = model.count - 1
                        , decrement = model.decrement + 1
                    }
            in
                ( newModel
                , storage newModel
                )

        Set newModel ->
            ( newModel, Cmd.none )

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


port storage : Model -> Cmd msg


port storageInput : (Model -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ jsMsgs mapJsMsg
        , storageInput Set
        ]


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
