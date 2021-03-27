module Board exposing (boardView)

import Html
import Html.Attributes exposing (class)


boardView : Html.Html msg
boardView =
    Html.table [ class "board" ]
        (List.range 0 2
            |> List.map groupView
        )


groupView : Int -> Html.Html msg
groupView groupIndex =
    Html.tbody
        [ class "border-4 border-gray-500" ]
        (List.range 0 2
            |> List.map (rowView groupIndex)
        )


rowView : Int -> Int -> Html.Html msg
rowView groupIndex rowInGroupIndex =
    Html.tr []
        (List.range 0 8
            |> List.map (cellView (groupIndex * 3 + rowInGroupIndex))
        )


cellView : Int -> Int -> Html.Html msg
cellView rowIndex colIndex =
    let
        className =
            if modBy 3 (colIndex + 1) == 0 then
                "cell-third"

            else
                "border-gray-300"

        classNames =
            [ "w-12", "h-12", "border-r-2", "border-b-2", className ]
    in
    Html.td
        [ class (String.join " " classNames) ]
        [ Html.text <| String.concat [ "(", String.fromInt rowIndex, ",", String.fromInt colIndex, ")" ] ]
