port module Main exposing (main)

import Board
import Browser
import Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    { boardModel : Board.Model
    }


type Msg
    = BoardMsg Board.Msg
    | ToggleDarkMode


board1 : Board.Board
board1 =
    Board.fromList <|
        [ [ 0, 0, 0, 0, 4, 0, 8, 7, 0 ]
        , [ 0, 0, 1, 0, 0, 0, 3, 4, 9 ]
        , [ 7, 4, 3, 8, 9, 0, 0, 5, 6 ]
        , [ 0, 0, 0, 9, 1, 0, 0, 2, 7 ]
        , [ 4, 0, 0, 0, 0, 7, 9, 1, 0 ]
        , [ 0, 0, 7, 0, 0, 4, 0, 0, 3 ]
        , [ 3, 7, 0, 0, 6, 0, 0, 9, 0 ]
        , [ 1, 2, 0, 5, 3, 8, 7, 0, 0 ]
        , [ 0, 6, 5, 0, 0, 0, 0, 3, 0 ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boardModel = Board.Model board1 Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardMsg boardMsg ->
            let
                newBoardModel =
                    Board.update boardMsg model.boardModel
            in
            ( { model | boardModel = newBoardModel }, Cmd.none )

        ToggleDarkMode ->
            ( model, sendMessage "toggle-dark-mode" )


view : Model -> Html.Html Msg
view model =
    Html.main_ [ class "p-5" ]
        [ Html.h1 [ class "text-3xl dark:text-white mb-5" ] [ Html.text "Elm Sudoku Solver" ]
        , Html.button
            [ class "border rounded py-2 px-4 mb-5 bg-gray-800 text-gray-50 dark:bg-gray-50 dark:text-gray-800", onClick ToggleDarkMode ]
            [ Html.text "Toggle Dark Mode" ]
        , Html.map BoardMsg (Board.view model.boardModel)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map BoardMsg (Board.subscriptions model.boardModel)


port sendMessage : String -> Cmd msg


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
