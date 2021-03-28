module Board exposing (Board, Cell(..), CellCoords(..), Msg(..), boardView, fromList)

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


type alias Board =
    Array (Array Cell)


type CellCoords
    = CellCoords ( Int, Int )


type Msg
    = CellClick CellCoords
    | KeyDown String


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


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


boardView : Board -> Maybe CellCoords -> Html.Html Msg
boardView board selectedCell =
    Html.table [ class "board" ]
        (List.range 0 2
            |> List.map (groupView board selectedCell)
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

        ( cellStr, cellTextClassName ) =
            cellMaybe
                |> Maybe.map
                    (\cell ->
                        case cell of
                            Cell val ->
                                ( String.fromInt val, "text-gray-600 dark:text-gray-50" )

                            CellUser val ->
                                ( String.fromInt val, "text-blue-500 dark:text-blue-300" )

                            CellEmpty ->
                                ( "", "" )
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
            [ "w-12 h-12 border-r-2 border-b-2 text-3xl hover:bg-gray-200 dark:hover:bg-gray-600 duration-150 ease-in", className, cellTextClassName, selectedCellClassName ]
    in
    Html.td
        [ class (String.join " " classNames), onClick (CellClick (CellCoords ( rowIndex, colIndex ))) ]
        [ Html.text cellStr ]
