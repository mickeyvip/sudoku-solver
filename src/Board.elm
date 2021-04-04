module Board exposing (Board, Model, Msg, fromList, subscriptions, update, view)

import Array exposing (Array)
import Browser.Events exposing (onKeyDown)
import Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode


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
    }


type Msg
    = CellClick CellCoords
    | CellKeyPressed CellKey


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


getCell : Int -> Int -> Board -> Maybe Cell
getCell rowIndex colIndex board =
    Array.get rowIndex board
        |> Maybe.andThen
            (\row ->
                Array.get colIndex row
            )


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
                getPreviousRowIndex : Int -> Int -> Maybe Int
                getPreviousRowIndex rowIndex colIndex =
                    model.board
                        |> Array.indexedMap Tuple.pair
                        |> Array.slice 0 rowIndex
                        |> Array.filter
                            (\( _, row ) ->
                                case Array.get colIndex row of
                                    Just (Cell _) ->
                                        False

                                    Nothing ->
                                        False

                                    _ ->
                                        True
                            )
                        |> (\indexedRows -> Array.get (Array.length indexedRows - 1) indexedRows)
                        |> Maybe.map Tuple.first

                getNextRowIndex : Int -> Int -> Maybe Int
                getNextRowIndex rowIndex colIndex =
                    model.board
                        |> Array.indexedMap Tuple.pair
                        |> Array.slice (rowIndex + 1) (Array.length model.board)
                        |> Array.filter
                            (\( _, row ) ->
                                case Array.get colIndex row of
                                    Just (Cell _) ->
                                        False

                                    Nothing ->
                                        False

                                    _ ->
                                        True
                            )
                        |> Array.get 0
                        |> Maybe.map Tuple.first

                getPreviousColIndex : Int -> Int -> Maybe Int
                getPreviousColIndex rowIndex colIndex =
                    model.board
                        |> Array.get rowIndex
                        |> Maybe.andThen
                            (\row ->
                                row
                                    |> Array.indexedMap Tuple.pair
                                    |> Array.slice 0 colIndex
                                    |> Array.filter
                                        (\( _, cell ) ->
                                            case cell of
                                                Cell _ ->
                                                    False

                                                _ ->
                                                    True
                                        )
                                    |> (\indexedCells -> Array.get (Array.length indexedCells - 1) indexedCells)
                                    |> Maybe.map Tuple.first
                            )

                getNextColIndex : Int -> Int -> Maybe Int
                getNextColIndex rowIndex colIndex =
                    model.board
                        |> Array.get rowIndex
                        |> Maybe.andThen
                            (\row ->
                                row
                                    |> Array.indexedMap Tuple.pair
                                    |> Array.slice (colIndex + 1) (Array.length row)
                                    |> Array.filter
                                        (\( _, cell ) ->
                                            case cell of
                                                Cell _ ->
                                                    False

                                                _ ->
                                                    True
                                        )
                                    |> (\indexedCells -> Array.get 0 indexedCells)
                                    |> Maybe.map Tuple.first
                            )

                newSelectedCell : Maybe CellCoords
                newSelectedCell =
                    model.selectedCell
                        |> Maybe.andThen
                            (\(CellCoords ( rowIndex, colIndex )) ->
                                case direction of
                                    Up ->
                                        case getPreviousRowIndex rowIndex colIndex of
                                            Just previousRowIndex ->
                                                Just (CellCoords ( previousRowIndex, colIndex ))

                                            _ ->
                                                Just (CellCoords ( rowIndex, colIndex ))

                                    Down ->
                                        case getNextRowIndex rowIndex colIndex of
                                            Just nextRowIndex ->
                                                Just (CellCoords ( nextRowIndex, colIndex ))

                                            _ ->
                                                Just (CellCoords ( rowIndex, colIndex ))

                                    Left ->
                                        case getPreviousColIndex rowIndex colIndex of
                                            Just previousColIndex ->
                                                Just (CellCoords ( rowIndex, previousColIndex ))

                                            _ ->
                                                Just (CellCoords ( rowIndex, colIndex ))

                                    Right ->
                                        case getNextColIndex rowIndex colIndex of
                                            Just nextColIndex ->
                                                Just (CellCoords ( rowIndex, nextColIndex ))

                                            _ ->
                                                Just (CellCoords ( rowIndex, colIndex ))
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
            in
            { model | board = newBoard }

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
            in
            { model | board = newBoard }


view : Model -> Html.Html Msg
view model =
    Html.table [ class "board" ]
        (List.range 0 2
            |> List.map (groupView model.board model.selectedCell)
        )


groupView : Board -> Maybe CellCoords -> Int -> Html.Html Msg
groupView board selectedCell groupIndex =
    Html.tbody
        [ class "border-4 border-gray-500 dark:border-gray-200" ]
        (List.range 0 2
            |> List.map (rowView board selectedCell groupIndex)
        )


rowView : Board -> Maybe CellCoords -> Int -> Int -> Html.Html Msg
rowView board selectedCell groupIndex rowInGroupIndex =
    let
        rowIndex : Int
        rowIndex =
            groupIndex * 3 + rowInGroupIndex
    in
    Html.tr []
        (List.range 0 8
            |> List.map (cellView board selectedCell rowIndex)
        )


cellView : Board -> Maybe CellCoords -> Int -> Int -> Html.Html Msg
cellView board selectedCell rowIndex colIndex =
    let
        cellMaybe =
            Array.get rowIndex board
                |> Maybe.andThen (\row -> Array.get colIndex row)

        ( cellStr, cellClassName ) =
            cellMaybe
                |> Maybe.map
                    (\cell ->
                        case cell of
                            Cell val ->
                                ( String.fromInt val, "text-gray-600 dark:text-gray-50" )

                            CellUser val ->
                                ( String.fromInt val, "hover:bg-gray-200 dark:hover:bg-gray-600 text-blue-500 dark:text-blue-300" )

                            CellEmpty ->
                                ( "", "hover:bg-gray-200 dark:hover:bg-gray-600 duration-150 ease-in" )
                    )
                |> Maybe.withDefault ( "", "" )

        className =
            if modBy 3 (colIndex + 1) == 0 then
                "cell-third"

            else
                "border-gray-400"

        selectedCellClassName =
            case selectedCell of
                Just (CellCoords ( rowIndexSelected, colIndexSelected )) ->
                    if rowIndex == rowIndexSelected && colIndex == colIndexSelected then
                        "bg-gray-300 dark:bg-gray-500"

                    else
                        ""

                Nothing ->
                    ""

        classNames =
            [ "w-12 h-12 border-r-2 border-b-2 text-3xl", className, cellClassName, selectedCellClassName ]
    in
    Html.td
        [ class (String.join " " classNames), onClick (CellClick (CellCoords ( rowIndex, colIndex ))) ]
        [ Html.text cellStr ]


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown cellKeyDecoder
