module Board exposing (Board, Cell, CellCoords(..), Model, Msg, boardCell, emptyBoard, emptyCell, fromList, getCell, getEmptyCellsCellCoords, initialModel, isBoardCell, isBoardFull, isBoardValid, isEmptyCell, isUserCell, setBoard, setCell, subscriptions, update, userCell, view)

import Array exposing (Array)
import Browser.Events exposing (onKeyDown)
import Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Set


type Cell
    = CellEmpty
    | Cell Int
    | CellUser Int


type Direction
    = Up
    | Down
    | Left
    | Right


type CellKey
    = DeleteKey
    | NavigationKey Direction
    | NumberKey Int


type alias Board =
    Array (Array Cell)


type CellCoords
    = CellCoords ( Int, Int )


type alias Model =
    { board : Board
    , selectedCell : Maybe CellCoords
    , valid : Bool
    }


type Msg
    = CellClick CellCoords
    | CellKeyPressed CellKey


emptyBoard : Board
emptyBoard =
    Array.repeat 9 (Array.repeat 9 CellEmpty)


initialModel : Model
initialModel =
    Model emptyBoard Nothing True



{- Cell query -}


isEmptyCell : Cell -> Bool
isEmptyCell cell =
    case cell of
        CellEmpty ->
            True

        _ ->
            False


isBoardCell : Cell -> Bool
isBoardCell cell =
    case cell of
        Cell _ ->
            True

        _ ->
            False


isUserCell : Cell -> Bool
isUserCell cell =
    case cell of
        CellUser _ ->
            True

        _ ->
            False



{- Cell creation -}


emptyCell : Cell
emptyCell =
    CellEmpty


boardCell : Int -> Maybe Cell
boardCell n =
    if n >= 1 && n <= 9 then
        Just (Cell n)

    else
        Nothing


userCell : Int -> Maybe Cell
userCell n =
    if n >= 1 && n <= 9 then
        Just (CellUser n)

    else
        Nothing



{- Board  creation -}


fromList : List (List Int) -> Board
fromList boardAsList =
    let
        intToCell : Int -> Cell
        intToCell int =
            if int == 0 then
                CellEmpty

            else
                Cell int
    in
    Array.fromList boardAsList
        |> Array.map
            (\rowList ->
                rowList
                    |> List.map intToCell
                    |> Array.fromList
            )


setBoard : Board -> Model -> Model
setBoard board model =
    let
        valid =
            isBoardValid board
    in
    { model | board = board, valid = valid }


setCell : Int -> Int -> Cell -> Board -> Board
setCell rowIndex colIndex newCell board =
    let
        updateCellOnBoard rowIndex_ colIndex_ row_ =
            Array.set rowIndex_ (Array.set colIndex_ newCell row_) board
    in
    Array.get rowIndex board
        |> Maybe.andThen
            (\row ->
                Array.get colIndex row
                    |> Maybe.andThen
                        (\cell ->
                            case cell of
                                CellEmpty ->
                                    Just <| updateCellOnBoard rowIndex colIndex row

                                CellUser _ ->
                                    Just <| updateCellOnBoard rowIndex colIndex row

                                _ ->
                                    Just <| board
                        )
            )
        |> Maybe.withDefault board



{- Board query -}


getCell : Int -> Int -> Board -> Maybe Cell
getCell rowIndex colIndex board =
    Array.get rowIndex board
        |> Maybe.andThen
            (\row ->
                Array.get colIndex row
            )


getSquareCoords : Int -> Int -> CellCoords
getSquareCoords rowIndex colIndex =
    CellCoords ( rowIndex // 3, colIndex // 3 )


getEmptyCellsCellCoords : Board -> Array CellCoords
getEmptyCellsCellCoords board =
    Array.foldl
        (\( rowIndex, row ) acc ->
            let
                rowWithIndex =
                    Array.indexedMap Tuple.pair row

                emptyCellWithIndex =
                    rowWithIndex
                        |> Array.filter (\( _, cell ) -> isEmptyCell cell)
                        |> Array.map (\( colIndex, _ ) -> CellCoords ( rowIndex, colIndex ))
            in
            Array.append acc emptyCellWithIndex
        )
        Array.empty
        (Array.indexedMap Tuple.pair board)


isBoardFull : Board -> Bool
isBoardFull board =
    let
        emptyCellsCount =
            board
                |> getEmptyCellsCellCoords
                |> Array.length
    in
    emptyCellsCount == 0


isBoardValid : Board -> Bool
isBoardValid board =
    let
        cellValue cell =
            case cell of
                Cell n ->
                    n

                CellUser n ->
                    n

                _ ->
                    0

        valid =
            List.range 0 8
                |> List.map
                    (\rowIndex ->
                        let
                            row_ =
                                List.range 0 8
                                    |> List.map
                                        (\colIndex ->
                                            board
                                                |> getCell rowIndex colIndex
                                                |> Maybe.withDefault CellEmpty
                                        )
                                    |> List.filter (not << isEmptyCell)
                                    |> List.map cellValue

                            col_ =
                                List.range 0 8
                                    |> List.map
                                        (\colIndex ->
                                            board
                                                |> getCell rowIndex colIndex
                                                |> Maybe.withDefault CellEmpty
                                        )
                                    |> List.filter (not << isEmptyCell)
                                    |> List.map cellValue

                            square_ =
                                List.range 0 8
                                    |> List.map
                                        (\colIndex ->
                                            board
                                                |> getCell (modBy 3 rowIndex * 3 + modBy 3 colIndex) (rowIndex // 3 * 3 + colIndex // 3)
                                                |> Maybe.withDefault CellEmpty
                                        )
                                    |> List.filter (not << isEmptyCell)
                                    |> List.map cellValue

                            rowInvalid =
                                Set.size (Set.fromList row_) /= List.length row_

                            colInvalid =
                                Set.size (Set.fromList col_) /= List.length col_

                            squareInvalid =
                                Set.size (Set.fromList square_) /= List.length square_
                        in
                        rowInvalid || colInvalid || squareInvalid
                    )
                |> List.any identity
                |> not
    in
    valid



{- Update -}


update : Msg -> Model -> Model
update msg model =
    case msg of
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
                                        model.selectedCell
                            )
            in
            { model | selectedCell = selectedCellNew }

        CellKeyPressed (NavigationKey direction) ->
            let
                boardLength =
                    Array.length model.board

                getPreviousRowIndex : Int -> Int
                getPreviousRowIndex rowIndex =
                    max 0 (rowIndex - 1)

                getNextRowIndex : Int -> Int
                getNextRowIndex rowIndex =
                    min (boardLength - 1) (rowIndex + 1)

                getPreviousColIndex : Int -> Int
                getPreviousColIndex colIndex =
                    max 0 (colIndex - 1)

                getNextColIndex : Int -> Int
                getNextColIndex colIndex =
                    min (boardLength - 1) (colIndex + 1)

                newSelectedCell : Maybe CellCoords
                newSelectedCell =
                    model.selectedCell
                        |> Maybe.andThen
                            (\(CellCoords ( rowIndex, colIndex )) ->
                                case direction of
                                    Up ->
                                        Just (CellCoords ( getPreviousRowIndex rowIndex, colIndex ))

                                    Down ->
                                        Just (CellCoords ( getNextRowIndex rowIndex, colIndex ))

                                    Left ->
                                        Just (CellCoords ( rowIndex, getPreviousColIndex colIndex ))

                                    Right ->
                                        Just (CellCoords ( rowIndex, getNextColIndex colIndex ))
                            )
            in
            { model | selectedCell = newSelectedCell }

        CellKeyPressed DeleteKey ->
            let
                newBoard =
                    model.selectedCell
                        |> Maybe.andThen
                            (\(CellCoords ( rowIndex, colIndex )) ->
                                getCell rowIndex colIndex model.board
                                    |> Maybe.map
                                        (\cell ->
                                            case cell of
                                                CellUser _ ->
                                                    setCell rowIndex colIndex CellEmpty model.board

                                                _ ->
                                                    model.board
                                        )
                            )
                        |> Maybe.withDefault model.board

                valid =
                    isBoardValid newBoard
            in
            { model | board = newBoard, valid = valid }

        CellKeyPressed (NumberKey n) ->
            let
                newBoard =
                    model.selectedCell
                        |> Maybe.andThen
                            (\(CellCoords ( rowIndex, colIndex )) ->
                                getCell rowIndex colIndex model.board
                                    |> Maybe.map
                                        (\cell ->
                                            case cell of
                                                CellEmpty ->
                                                    setCell rowIndex colIndex (CellUser n) model.board

                                                CellUser _ ->
                                                    setCell rowIndex colIndex (CellUser n) model.board

                                                _ ->
                                                    model.board
                                        )
                            )
                        |> Maybe.withDefault model.board

                valid =
                    isBoardValid newBoard
            in
            { model | board = newBoard, valid = valid }



{- Views -}


view : Model -> Html.Html Msg
view model =
    Html.table [ class "board" ]
        (List.range 0 2
            |> List.map (groupView model.board model.selectedCell)
        )


groupView : Board -> Maybe CellCoords -> Int -> Html.Html Msg
groupView board selectedCellCoordsMaybe groupIndex =
    Html.tbody
        [ class "border-4 border-gray-500 dark:border-gray-200" ]
        (List.range 0 2
            |> List.map (rowView board selectedCellCoordsMaybe groupIndex)
        )


rowView : Board -> Maybe CellCoords -> Int -> Int -> Html.Html Msg
rowView board selectedCellCoordsMaybe groupIndex rowInGroupIndex =
    let
        rowIndex : Int
        rowIndex =
            groupIndex * 3 + rowInGroupIndex
    in
    Html.tr []
        (List.range 0 8
            |> List.map (cellView board selectedCellCoordsMaybe rowIndex)
        )


hoverCellClassName : String
hoverCellClassName =
    "hover:bg-gray-200 dark:hover:bg-gray-600"


selectedCellClassName : String
selectedCellClassName =
    "bg-blue-100 dark:bg-gray-400"


cellView : Board -> Maybe CellCoords -> Int -> Int -> Html.Html Msg
cellView board selectedCellCoordsMaybe rowIndex colIndex =
    let
        currentCellMaybe : Maybe Cell
        currentCellMaybe =
            getCell rowIndex colIndex board

        selectedCellMaybe : Maybe Cell
        selectedCellMaybe =
            selectedCellCoordsMaybe
                |> Maybe.andThen
                    (\(CellCoords ( rowSelectedIndex, colSelectedIndex )) ->
                        getCell rowSelectedIndex colSelectedIndex board
                    )

        cellOnPath : Bool
        cellOnPath =
            case selectedCellCoordsMaybe of
                Just (CellCoords ( rowSelectedIndex, colSelectedIndex )) ->
                    let
                        sameRow =
                            rowIndex == rowSelectedIndex

                        sameCol =
                            colIndex == colSelectedIndex

                        sameSquare =
                            getSquareCoords rowIndex colIndex == getSquareCoords rowSelectedIndex colSelectedIndex
                    in
                    if rowIndex == rowSelectedIndex && colIndex == colSelectedIndex then
                        False

                    else
                        sameRow || sameCol || sameSquare

                Nothing ->
                    False

        cellWithSameValue : Bool
        cellWithSameValue =
            Maybe.map2
                (\currentCellValue selectedCell ->
                    case selectedCell of
                        CellEmpty ->
                            False

                        Cell selectedCellValue ->
                            case currentCellValue of
                                CellEmpty ->
                                    False

                                Cell cellValue ->
                                    selectedCellValue == cellValue

                                CellUser cellUserValue ->
                                    selectedCellValue == cellUserValue

                        CellUser selectedCellValue ->
                            case currentCellValue of
                                CellEmpty ->
                                    False

                                Cell cellValue ->
                                    selectedCellValue == cellValue

                                CellUser cellUserValue ->
                                    selectedCellValue == cellUserValue
                )
                currentCellMaybe
                selectedCellMaybe
                |> Maybe.withDefault False

        cellOnPathClass : String
        cellOnPathClass =
            if cellOnPath then
                "bg-gray-200 dark:bg-gray-600"

            else
                ""

        ( cellStr, cellClass ) =
            currentCellMaybe
                |> Maybe.map
                    (\cell ->
                        case cell of
                            Cell val ->
                                ( String.fromInt val, "text-gray-600 dark:text-gray-50" )

                            CellUser val ->
                                ( String.fromInt val, "text-blue-500 dark:text-blue-300" )

                            CellEmpty ->
                                ( "", "duration-100 ease-in" )
                    )
                |> Maybe.withDefault ( "", "" )

        cellBorderClass : String
        cellBorderClass =
            if modBy 3 (colIndex + 1) == 0 then
                "cell-third"

            else
                "border-gray-400"

        selectedCellClass : String
        selectedCellClass =
            case selectedCellCoordsMaybe of
                Just (CellCoords ( rowIndexSelected, colIndexSelected )) ->
                    if (rowIndex == rowIndexSelected && colIndex == colIndexSelected) || cellWithSameValue then
                        selectedCellClassName

                    else
                        hoverCellClassName

                Nothing ->
                    ""

        classNames : List String
        classNames =
            [ "w-12 h-12 border-r-2 border-b-2 text-3xl", cellOnPathClass, cellBorderClass, cellClass, selectedCellClass ]
    in
    Html.td
        [ class (String.join " " classNames), onClick (CellClick (CellCoords ( rowIndex, colIndex ))) ]
        [ Html.text cellStr ]



{- Events and decoders -}


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

                    "Backspace" ->
                        Decode.succeed DeleteKey

                    "ArrowUp" ->
                        Decode.succeed (NavigationKey Up)

                    "ArrowDown" ->
                        Decode.succeed (NavigationKey Down)

                    "ArrowLeft" ->
                        Decode.succeed (NavigationKey Left)

                    "ArrowRight" ->
                        Decode.succeed (NavigationKey Right)

                    _ ->
                        Decode.fail "Unsupported key pressed"
            )
        |> Decode.map CellKeyPressed


cellKeyDecoder : Decode.Decoder Msg
cellKeyDecoder =
    Decode.oneOf [ cellKeyNumberDecoder, cellKeyActionDecoder ]



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown cellKeyDecoder
