port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)
import Json.Decode
import Json.Encode


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
            ( { model
                | todos = model.todo :: model.todos
                , todo = { newTodo | identifier = model.nextIdentifier }
                , nextIdentifier = model.nextIdentifier + 1
              }
            , Cmd.none
            )

        Complete todo ->
            ( updateTodo model todo True, Cmd.none )

        Uncomplete todo ->
            ( updateTodo model todo False, Cmd.none )

        Delete todo ->
            ( { model
                | todos = List.filter (\mappedTodo -> todo.identifier /= mappedTodo.identifier) model.todos
              }
            , Cmd.none
            )

        ClearCompleted ->
            ( { model
                | todos = List.filter (\todo -> not todo.completed) model.todos
              }
            , Cmd.none
            )

        Update text ->
            let
                todo =
                    model.todo
            in
                ( { model | todo = { todo | title = text } }, Cmd.none )

        Filter filterState ->
            ( { model | filter = filterState }, Cmd.none )

        Set newModel ->
            ( newModel, Cmd.none )


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
        , subscriptions = (\_ -> Sub.none)
        }


port storageInput : (Json.Decode.Value -> msg) -> Sub msg


port storage : Json.Encode.Value -> Cmd msg
