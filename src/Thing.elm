module Thing exposing (Element(..), HtmlConfig, Plugin, RenderPoint, button, col, defaultElAttributes, el, html, row, text)

import Browser exposing (element)
import Html
import Html.Attributes


type Element msg
    = None
    | Text String
    | El ElAttributes (Element msg)
    | Row (List (Element msg))
    | Col (List (Element msg))
    | Button (Element msg)


type alias ElAttributes =
    { fontColor : String
    }


defaultElAttributes : ElAttributes
defaultElAttributes =
    { fontColor = "black" }


type alias HtmlConfig msg =
    { plugins : List (Plugin msg)
    }


type alias Plugin msg =
    { renderPoint : RenderPoint msg
    }


type alias RenderPoint msg =
    Element msg -> Element msg


html : HtmlConfig msg -> Element msg -> Html.Html msg
html config preElement =
    let
        postElement =
            config.plugins |> List.foldl (\item acc -> preProcess item.renderPoint acc) preElement

        preProcess func element =
            func <|
                case element of
                    None ->
                        None

                    Text txt ->
                        Text txt

                    El attr child ->
                        El attr (preProcess func child)

                    Row children ->
                        Row (children |> List.map (preProcess func))

                    Col children ->
                        Col (children |> List.map (preProcess func))

                    Button child ->
                        Button (preProcess func child)

        render element =
            case element of
                None ->
                    Html.text ""

                Text txt ->
                    Html.div [] [ Html.text txt ]

                El attr child ->
                    Html.div [ style "color" attr.fontColor ] [ render child ]

                Row children ->
                    Html.div [ style "display" "flex", style "gap" "16px" ]
                        (List.map render children)

                Col children ->
                    Html.div [ style "display" "flex", style "gap" "16px", style "flex-direction" "column" ]
                        (List.map render children)

                Button child ->
                    Html.button [] [ render child ]
    in
    render postElement


el : ElAttributes -> Element msg -> Element msg
el attr child =
    El attr child


text : String -> Element msg
text str =
    Text str


row : List (Element msg) -> Element msg
row children =
    Row children


col : List (Element msg) -> Element msg
col children =
    Col children


button : Element msg -> Element msg
button child =
    Button child


style : String -> String -> Html.Attribute msg
style name value =
    Html.Attributes.style name value
