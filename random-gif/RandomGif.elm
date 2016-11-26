module RandomGif
    exposing
        ( Model
        , Msg
        , init
        , update
        , view
        )

import Html exposing (..)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Json


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type alias Model =
    { topic : String
    , gifUrl : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "dogs" "assets/waiting.gif"
    , Cmd.none
    )


type Msg
    = MorePlease
    | ChangeTopic String
    | NewGif (Result Http.Error String)


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

        request =
            Http.get url decodeGifUrl
    in
        Http.send NewGif request


decodeGifUrl : Json.Decoder String
decodeGifUrl =
    Json.at [ "data", "image_url" ] Json.string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        ChangeTopic newTopic ->
            ( { model | topic = newTopic }, Cmd.none )

        NewGif (Ok newUrl) ->
            ( { model | gifUrl = newUrl }, Cmd.none )

        NewGif (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [ headerStyle ] [ text model.topic ]
        , input [ value model.topic, onInput ChangeTopic ] []
        , button
            [ onClick MorePlease ]
            [ text "More Please!" ]
        , div [] [ img [ imgStyle model.gifUrl ] [] ]
        ]


headerStyle : Attribute Msg
headerStyle =
    style
        [ "width" => "200px"
        , "text-align" => "center"
        ]


imgStyle : String -> Attribute Msg
imgStyle url =
    style
        [ "display" => "inline-block"
        , "width" => "200px"
        , "height" => "200px"
        , "background-position" => "center center"
        , "background-size" => "cover"
        , "background-image" => ("url('" ++ url ++ "')")
        ]
