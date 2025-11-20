module Basic exposing (..)

import Html
import Thing exposing (..)


superTextRenderer : Plugin msg
superTextRenderer =
    { renderPoint =
        \e ->
            case e of
                Text _ ->
                    row [ text "{", e, text "}" ]

                _ ->
                    e
    }


superRenderer : Plugin msg
superRenderer =
    { renderPoint = \e -> col [ text "<", e, text ">" ] }


main : Html.Html msg
main =
    Thing.html { plugins = [ superRenderer, superTextRenderer ] } <|
        row
            [ text "Hello"
            , el { fontColor = "red" } <| text "World"
            , col [ text "A", text "B", button <| text "C" ]
            , button <| col [ text "1", text "2" ]
            ]
