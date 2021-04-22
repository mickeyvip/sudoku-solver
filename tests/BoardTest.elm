module BoardTest exposing (..)

import Array
import Board exposing (CellCoords(..), getEmptyCellsCellCoords, isEmptyCell)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


boardPartialValid : Board.Board
boardPartialValid =
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
        |> Board.fromList


boardFullValid : Board.Board
boardFullValid =
    [ [ 2, 9, 6, 3, 4, 5, 8, 7, 1 ]
    , [ 5, 8, 1, 7, 2, 6, 3, 4, 9 ]
    , [ 7, 4, 3, 8, 9, 1, 2, 5, 6 ]
    , [ 6, 5, 8, 9, 1, 3, 4, 2, 7 ]
    , [ 4, 3, 2, 6, 8, 7, 9, 1, 5 ]
    , [ 9, 1, 7, 2, 5, 4, 6, 8, 3 ]
    , [ 3, 7, 4, 1, 6, 2, 5, 9, 8 ]
    , [ 1, 2, 9, 5, 3, 8, 7, 6, 4 ]
    , [ 8, 6, 5, 4, 7, 9, 1, 3, 2 ]
    ]
        |> Board.fromList


boardPartialInvalid : Board.Board
boardPartialInvalid =
    [ [ 0, 0, 0, 0, 4, 0, 8, 7, 3 ]
    , [ 0, 0, 1, 0, 0, 0, 3, 4, 9 ]
    , [ 7, 4, 3, 8, 9, 0, 0, 5, 6 ]
    , [ 0, 0, 0, 9, 1, 0, 0, 2, 7 ]
    , [ 4, 0, 0, 0, 0, 7, 9, 1, 0 ]
    , [ 0, 0, 7, 0, 0, 4, 0, 0, 3 ]
    , [ 3, 7, 0, 0, 6, 0, 0, 9, 0 ]
    , [ 1, 2, 0, 5, 3, 8, 7, 0, 0 ]
    , [ 0, 6, 5, 0, 0, 0, 0, 3, 0 ]
    ]
        |> Board.fromList


boardFullInvalid : Board.Board
boardFullInvalid =
    [ [ 2, 9, 6, 3, 4, 5, 8, 7, 3 ]
    , [ 5, 8, 1, 7, 2, 6, 3, 4, 9 ]
    , [ 7, 4, 3, 8, 9, 1, 2, 5, 6 ]
    , [ 6, 5, 8, 9, 1, 3, 4, 2, 7 ]
    , [ 4, 3, 2, 6, 8, 7, 9, 1, 5 ]
    , [ 9, 1, 7, 2, 5, 4, 6, 8, 3 ]
    , [ 3, 7, 4, 1, 6, 2, 5, 9, 8 ]
    , [ 1, 2, 9, 5, 3, 8, 7, 6, 4 ]
    , [ 8, 6, 5, 4, 7, 9, 1, 3, 2 ]
    ]
        |> Board.fromList


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
                    cell01User =
                        Board.userCell 1
                            |> Maybe.withDefault Board.emptyCell

                    board =
                        Board.setCell 0
                            1
                            cell01User
                            boardPartialValid

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
        , test "gets correct empty cells coords" <|
            \_ ->
                let
                    emptyCellsCoords =
                        boardPartialValid
                            |> getEmptyCellsCellCoords
                            |> Array.toList
                in
                Expect.all
                    [ \subject -> Expect.equal 43 (List.length subject)
                    , \subject ->
                        case List.head subject of
                            Just (CellCoords ( 0, 0 )) ->
                                Expect.pass

                            _ ->
                                Expect.fail "Empty cell (0, 0) should exist"
                    , \subject ->
                        case List.member (CellCoords ( 7, 8 )) subject of
                            True ->
                                Expect.pass

                            _ ->
                                Expect.fail "Empty cell (7, 8) should exist"
                    ]
                    emptyCellsCoords
        , test "partial valid board" <|
            \_ ->
                Expect.all
                    [ \subject -> Expect.false "board should not be full" (Board.isBoardFull subject)
                    , \subject -> Expect.true "board should be valid" (Board.isBoardValid subject)
                    ]
                    boardPartialValid
        , test "full valid board" <|
            \_ ->
                Expect.all
                    [ \subject -> Expect.true "board should be full" (Board.isBoardFull subject)
                    , \subject -> Expect.true "board should be valid" (Board.isBoardValid subject)
                    ]
                    boardFullValid
        , test "partial invalid board" <|
            \_ ->
                Expect.all
                    [ \subject -> Expect.false "board should not be full" (Board.isBoardFull subject)
                    , \subject -> Expect.false "board should be invalid" (Board.isBoardValid subject)
                    ]
                    boardPartialInvalid
        , test "full invalid board" <|
            \_ ->
                Expect.all
                    [ \subject -> Expect.true "board should be full" (Board.isBoardFull subject)
                    , \subject -> Expect.false "board should be invalid" (Board.isBoardValid subject)
                    ]
                    boardFullInvalid
        ]
