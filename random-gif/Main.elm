module Main exposing (..)

import RandomGif exposing (..)
import Html exposing (program)


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


subs : Model -> Sub Msg
subs model =
    Sub.none
