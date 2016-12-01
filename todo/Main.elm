port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)
import Json.Decode
import Json.Encode
import Json.Decode.Pipeline as Pipeline


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    , identifier : Int
    }


type FilterState
    = All
    | Active
    | Completed


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    , nextIdentifier : Int
    }


type Msg
    = Add
    | Complete Todo
    | Uncomplete Todo
    | Delete Todo
    | ClearCompleted
    | Update String
    | Filter FilterState
    | Set Model
    | LogDecodeError String


newTodo : Todo
newTodo =
    { title = ""
    , completed = False
    , editing = False
    , identifier = 0
    }


init : ( Model, Cmd Msg )
init =
    ( { todos =
            [ { title = "Milk and Cookies"
              , completed = True
              , editing = False
              , identifier = 1
              }
            ]
      , todo = { newTodo | identifier = 2 }
      , filter = All
      , nextIdentifier = 3
      }
    , Cmd.none
    )


updateTodo : Model -> Todo -> Bool -> Model
updateTodo model todo complete =
    let
        changeTodo thisTodo =
            if thisTodo.identifier == todo.identifier then
                { todo | completed = complete }
            else
                thisTodo
    in
        { model
            | todos = List.map changeTodo model.todos
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            let
                newModel =
                    { model
                        | todos = model.todo :: model.todos
                        , todo = { newTodo | identifier = model.nextIdentifier }
                        , nextIdentifier = model.nextIdentifier + 1
                    }
            in
                ( newModel
                , sendToStorage newModel
                )

        Complete todo ->
            let
                newModel =
                    updateTodo model todo True
            in
                ( newModel, sendToStorage newModel )

        Uncomplete todo ->
            let
                newModel =
                    updateTodo model todo False
            in
                ( newModel, sendToStorage newModel )

        Delete todo ->
            let
                newModel =
                    { model
                        | todos = List.filter (\mappedTodo -> todo.identifier /= mappedTodo.identifier) model.todos
                    }
            in
                ( newModel
                , sendToStorage newModel
                )

        ClearCompleted ->
            let
                newModel =
                    { model
                        | todos = List.filter (\todo -> not todo.completed) model.todos
                    }
            in
                ( newModel
                , sendToStorage newModel
                )

        Update text ->
            let
                todo =
                    model.todo

                newModel =
                    { model | todo = { todo | title = text } }
            in
                ( newModel, sendToStorage newModel )

        Filter filterState ->
            let
                newModel =
                    { model | filter = filterState }
            in
                ( newModel, sendToStorage newModel )

        Set newModel ->
            ( newModel, Cmd.none )

        LogDecodeError msg ->
            ( Tuple.first init, error msg )


enterKey : Int -> Json.Decode.Decoder Int
enterKey code =
    if code == 13 then
        Json.Decode.succeed code
    else
        Json.Decode.fail "not the enter key"


onEnterKeyPress : Msg -> Attribute Msg
onEnterKeyPress msg =
    on "keypress" (Json.Decode.map (always msg) (keyCode |> Json.Decode.andThen enterKey))


filteredTodos : Model -> List Todo
filteredTodos model =
    let
        todoFilter =
            case model.filter of
                All ->
                    (\_ -> True)

                Active ->
                    (\todo -> not todo.completed)

                Completed ->
                    (\todo -> todo.completed)
    in
        List.filter todoFilter model.todos


view : Model -> Html Msg
view model =
    div []
        [ headerView model
        , mainView model
        , footerView model
        ]


headerView : Model -> Html Msg
headerView model =
    section
        [ class "todoapp"
        ]
        [ header [ class "header" ]
            [ h1 [] [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , value model.todo.title
                , autofocus True
                , onEnterKeyPress Add
                , onInput Update
                ]
                []
            ]
        ]


mainView : Model -> Html Msg
mainView model =
    section [ class "main" ]
        [ ul [ class "todo-list" ]
            (List.map todoView (filteredTodos model))
        ]


todoView : Todo -> Html Msg
todoView todo =
    let
        handleComplete =
            case todo.completed of
                True ->
                    (\_ -> Uncomplete todo)

                False ->
                    (\_ -> Complete todo)
    in
        li [ classList [ ( "completed", todo.completed ) ] ]
            [ div [ class "view" ]
                [ input
                    [ class "toggle"
                    , type_ "checkbox"
                    , checked todo.completed
                    , onCheck handleComplete
                    ]
                    []
                , label [] [ text todo.title ]
                , button
                    [ class "destroy"
                    , onClick (Delete todo)
                    ]
                    []
                ]
            ]


footerView : Model -> Html Msg
footerView model =
    footer [ class "footer" ]
        [ span [ class "todo-count" ]
            [ strong [] [ text (toString (List.length (List.filter (\todo -> not todo.completed) model.todos))) ]
            , text " items left"
            ]
        , ul [ class "filters" ]
            [ filterItemView model All
            , filterItemView model Active
            , filterItemView model Completed
            ]
        , button
            [ class "clear-completed"
            , onClick ClearCompleted
            ]
            [ text "Clear Completed" ]
        ]


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList [ ( "selected", (model.filter == filterState) ) ]
            , href "#"
            , onClick (Filter filterState)
            ]
            [ text (toString filterState) ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    storageInput mapStorageInput


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "todos", Json.Encode.list (List.map encodeTodo model.todos) )
        , ( "todo", encodeTodo model.todo )
        , ( "filter", encodeFilterState model.filter )
        , ( "nextIdentifier", Json.Encode.int model.nextIdentifier )
        ]


encodeTodo : Todo -> Json.Encode.Value
encodeTodo todo =
    Json.Encode.object
        [ ( "title", Json.Encode.string todo.title )
        , ( "completed", Json.Encode.bool todo.completed )
        , ( "editing", Json.Encode.bool todo.editing )
        , ( "identifier", Json.Encode.int todo.identifier )
        ]


encodeFilterState : FilterState -> Json.Encode.Value
encodeFilterState filterState =
    Json.Encode.string <| toString filterState


modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Pipeline.decode Model
        |> Pipeline.required "todos" (Json.Decode.list todoDecoder)
        |> Pipeline.required "todo" todoDecoder
        |> Pipeline.required "filter" filterStateDecoder
        |> Pipeline.required "nextIdentifier" Json.Decode.int


todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
    Pipeline.decode Todo
        |> Pipeline.required "title" Json.Decode.string
        |> Pipeline.required "completed" Json.Decode.bool
        |> Pipeline.required "editing" Json.Decode.bool
        |> Pipeline.required "identifier" Json.Decode.int


filterStateDecoder : Json.Decode.Decoder FilterState
filterStateDecoder =
    let
        decodeToFilterState string =
            case string of
                "All" ->
                    Json.Decode.succeed All

                "Active" ->
                    Json.Decode.succeed Active

                "Completed" ->
                    Json.Decode.succeed Completed

                _ ->
                    Json.Decode.fail <| "Not a valid FilterState: " ++ string
    in
        Json.Decode.string |> Json.Decode.andThen decodeToFilterState


decodeModel : Json.Decode.Value -> Result String Model
decodeModel modelJson =
    Json.Decode.decodeValue modelDecoder modelJson


mapStorageInput : Json.Decode.Value -> Msg
mapStorageInput modelJson =
    case (decodeModel modelJson) of
        Ok model ->
            Set model

        Err msg ->
            LogDecodeError msg


sendToStorage : Model -> Cmd Msg
sendToStorage model =
    encodeModel model |> storage


port storageInput : (Json.Decode.Value -> msg) -> Sub msg


port storage : Json.Encode.Value -> Cmd msg


port error : String -> Cmd msg
