module Main exposing (main)

import Board exposing (boardView)
import Html
import Html.Attributes exposing (class)


main : Html.Html msg
main =
    Html.main_ [ class "p-5" ]
        [ Html.h1 [ class "text-2xl" ] [ Html.text "Elm Sudoku Solver" ]
        , boardView
        ]
