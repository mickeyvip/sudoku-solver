module Main exposing (main)

import Array
import Board exposing (Board, Cell(..), CellCoords(..), Msg(..), boardView, fromList)
import Browser
import Browser.Events exposing (onKeyDown)
import Html
import Html.Attributes exposing (class)
import Json.Decode as Decode


type alias Model =
    { board : Board
    , selectedCell : Maybe CellCoords
    }


type Msg
    = BoardMsg Board.Msg
    | CellClicked Int


boardEmpty : Board
boardEmpty =
    Array.repeat 9 (Array.repeat 9 CellEmpty)


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
                    ( { model | selectedCell = Just (CellCoords ( rowIndex, colIndex )) }, Cmd.none )

                KeyDown key ->
                    ( model, Cmd.none )

        CellClicked n ->
            let
                newBoard =
                    case model.selectedCell of
                        Just (CellCoords ( rowIndex, colIndex )) ->
                            Array.get rowIndex model.board
                                |> Maybe.map
                                    (\row ->
                                        case Array.get colIndex row of
                                            Just CellEmpty ->
                                                Array.set rowIndex (Array.set colIndex (Cell n) row) model.board

                                            _ ->
                                                model.board
                                    )
                                |> Maybe.withDefault model.board

                        Nothing ->
                            model.board
            in
            ( { model | board = newBoard }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.main_ [ class "p-5" ]
        [ Html.h1 [ class "text-2xl" ] [ Html.text "Elm Sudoku Solver" ]
        , Html.map BoardMsg (boardView model.board model.selectedCell)
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case String.toInt key of
                    Just n ->
                        if n >= 1 && n <= 9 then
                            Decode.succeed n

                        else
                            Decode.fail "Integer is not between 1 and 9"

                    Nothing ->
                        Decode.fail "Not an integer"
            )
        |> Decode.map CellClicked


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.selectedCell of
        Just _ ->
            onKeyDown keyDecoder

        Nothing ->
            Sub.none


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
