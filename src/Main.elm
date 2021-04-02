port module Main exposing (main)

import Array
import Board exposing (Board, Cell(..), CellCoords(..), Msg(..), boardView, fromList, getCell, setCell)
import Browser
import Browser.Events exposing (onKeyDown)
import Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode


type alias Model =
    { board : Board
    , selectedCell : Maybe CellCoords
    }


type CellKey
    = DeleteKey
    | UpKey
    | DownKey
    | LeftKey
    | RightKey
    | NumberKey Int


type Msg
    = BoardMsg Board.Msg
    | CellKeyPressed CellKey
    | ToggleDarkMode


board1 : Board
board1 =
    fromList <|
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
    ( { board = board1
      , selectedCell = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardMsg boardMsg ->
            case boardMsg of
                CellClick (CellCoords ( rowIndex, colIndex )) ->
                    let
                        selectedCellNew =
                            Array.get rowIndex model.board
                                |> Maybe.andThen
                                    (\row ->
                                        case Array.get colIndex row of
                                            Just CellEmpty ->
                                                Just (CellCoords ( rowIndex, colIndex ))

                                            Just (CellUser _) ->
                                                Just (CellCoords ( rowIndex, colIndex ))

                                            _ ->
                                                Nothing
                                    )
                    in
                    ( { model | selectedCell = selectedCellNew }, Cmd.none )

                KeyDown key ->
                    ( model, Cmd.none )

        CellKeyPressed cellKey ->
            let
                newSelectedCell =
                    model.selectedCell
                        |> Maybe.andThen
                            (\(CellCoords ( rowIndex, colIndex )) ->
                                case cellKey of
                                    UpKey ->
                                        if rowIndex > 0 then
                                            Just <| CellCoords ( rowIndex - 1, colIndex )

                                        else
                                            model.selectedCell

                                    DownKey ->
                                        if rowIndex < 8 then
                                            Just <| CellCoords ( rowIndex + 1, colIndex )

                                        else
                                            model.selectedCell

                                    LeftKey ->
                                        if colIndex > 0 then
                                            Just <| CellCoords ( rowIndex, colIndex - 1 )

                                        else
                                            model.selectedCell

                                    RightKey ->
                                        if colIndex < 8 then
                                            Just <| CellCoords ( rowIndex, colIndex + 1 )

                                        else
                                            model.selectedCell

                                    _ ->
                                        model.selectedCell
                            )

                newBoard =
                    model.selectedCell
                        |> Maybe.andThen
                            (\(CellCoords ( rowIndex, colIndex )) ->
                                getCell rowIndex colIndex model.board
                                    |> Maybe.andThen
                                        (\cell ->
                                            case cell of
                                                CellEmpty ->
                                                    case cellKey of
                                                        NumberKey n ->
                                                            Just <| setCell rowIndex colIndex (CellUser n) model.board

                                                        _ ->
                                                            Just model.board

                                                CellUser _ ->
                                                    case cellKey of
                                                        NumberKey n ->
                                                            Just <| setCell rowIndex colIndex (CellUser n) model.board

                                                        DeleteKey ->
                                                            Just <| setCell rowIndex colIndex CellEmpty model.board

                                                        _ ->
                                                            Just <| model.board

                                                _ ->
                                                    Just <| model.board
                                        )
                            )
                        |> Maybe.withDefault model.board
            in
            ( { model | board = newBoard, selectedCell = newSelectedCell }, Cmd.none )

        ToggleDarkMode ->
            ( model, sendMessage "toggle-dark-mode" )


view : Model -> Html.Html Msg
view model =
    Html.main_ [ class "p-5" ]
        [ Html.h1 [ class "text-3xl dark:text-white mb-5" ] [ Html.text "Elm Sudoku Solver" ]
        , Html.button
            [ class "border rounded py-2 px-4 mb-5 bg-gray-800 text-gray-50 dark:bg-gray-50 dark:text-gray-800", onClick ToggleDarkMode ]
            [ Html.text "Toggle Dark Mode" ]
        , Html.map BoardMsg (boardView model.board model.selectedCell)
        ]


cellKeyNumberDecoder : Decode.Decoder Msg
cellKeyNumberDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case String.toInt key of
                    Just n ->
                        if n >= 1 && n <= 9 then
                            Decode.succeed (NumberKey n)

                        else
                            Decode.fail "Integer is not between 1 and 9"

                    Nothing ->
                        Decode.fail "Not an integer"
            )
        |> Decode.map CellKeyPressed


cellKeyActionDecoder : Decode.Decoder Msg
cellKeyActionDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "Delete" ->
                        Decode.succeed DeleteKey

                    "ArrowUp" ->
                        Decode.succeed UpKey

                    "ArrowDown" ->
                        Decode.succeed DownKey

                    "ArrowLeft" ->
                        Decode.succeed LeftKey

                    "ArrowRight" ->
                        Decode.succeed RightKey

                    _ ->
                        Decode.fail "Unsupported key pressed"
            )
        |> Decode.map CellKeyPressed


cellKeyDecoder : Decode.Decoder Msg
cellKeyDecoder =
    Decode.oneOf [ cellKeyNumberDecoder, cellKeyActionDecoder ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.selectedCell of
        Just _ ->
            onKeyDown cellKeyDecoder

        Nothing ->
            Sub.none


port sendMessage : String -> Cmd msg


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
