module BoardTest exposing (..)

import Array
import Board exposing (isEmptyCell)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Sudoku Board"
        [ test "can create an empty board" <|
            \_ ->
                let
                    emptyBoard =
                        Board.emptyBoard

                    emptyBoardList =
                        List.map
                            Array.toList
                            (Array.toList emptyBoard)
                in
                List.all (List.all isEmptyCell) emptyBoardList
                    |> Expect.true "expected all the cells to be of type EmptyCell"
        , test "can create a Board from List (List Int)" <|
            \_ ->
                let
                    boardList =
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

                    cell01User =
                        Board.userCell 1
                            |> Maybe.withDefault Board.emptyCell

                    board =
                        Board.setCell 0
                            1
                            cell01User
                            (Board.fromList boardList)

                    cell00Empty =
                        board
                            |> Board.getCell 0 0
                            |> Maybe.map Board.isEmptyCell
                            |> Maybe.withDefault False

                    cell21Cell =
                        board
                            |> Board.getCell 2 1
                            |> Maybe.map Board.isBoardCell
                            |> Maybe.withDefault False

                    cell01CellUser =
                        board
                            |> Board.getCell 0 1
                            |> Maybe.map Board.isUserCell
                            |> Maybe.withDefault False
                in
                List.all identity [ cell00Empty, cell21Cell, cell01CellUser ]
                    |> Expect.true "all cells are correct"
        ]
