module Thing exposing (Element(..), HtmlConfig, Model, Msg, Plugin, RenderPoint, button, col, defaultElAttributes, el, init, lineInput, row, text, toHtml, update)

import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events


type Element msg
    = None
    | Text String
    | El ElAttributes (Element msg)
    | Row (List (Element msg))
    | Col (List (Element msg))
    | Button (Element msg)
    | LineInput (String -> msg) String


type alias ElAttributes =
    { fontColor : String
    , backgroundColor : String
    , padding : Int
    , rounding : Int
    }


defaultElAttributes : ElAttributes
defaultElAttributes =
    { fontColor = "black"
    , backgroundColor = "white"
    , padding = 0
    , rounding = 0
    }


type alias HtmlConfig msg =
    { plugins : List (Plugin msg)
    , intoMsg : Msg -> msg
    }


type alias Plugin msg =
    { renderPoint : RenderPoint msg
    }


type alias RenderPoint msg =
    Element msg -> Element msg



-- type alias UpatePoint =
--     Msg -> Msg


type alias Model =
    { hovering : Dict String Bool
    }


type Msg
    = NoOp
    | Hover String
    | UnHover String


init : Model
init =
    { hovering = Dict.empty }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Hover key ->
            { model | hovering = Dict.insert key True model.hovering }

        UnHover key ->
            { model | hovering = Dict.remove key model.hovering }


toHtml : HtmlConfig msg -> Model -> Element msg -> Html.Html msg
toHtml config model preElement =
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

                    LineInput onChange str ->
                        LineInput onChange str

        render element key =
            let
                renderIndex =
                    \i c -> render c (key ++ "/" ++ String.fromInt i)

                hovering =
                    model.hovering |> Dict.get key |> Maybe.withDefault False
            in
            case element of
                None ->
                    Html.text ""

                Text txt ->
                    Html.div [] [ Html.text txt ]

                El attr child ->
                    Html.div
                        ([ style "color" attr.fontColor
                         , style "background-color" attr.backgroundColor
                         , style "padding" (String.fromInt attr.padding ++ "px")
                         , style "border-radius" (String.fromInt attr.rounding ++ "px")
                         , Html.Events.onMouseEnter (config.intoMsg <| Hover key)
                         , Html.Events.onMouseLeave (config.intoMsg <| UnHover key)
                         ]
                            ++ (if hovering then
                                    [ style "outline" "5px solid greenyellow" ]

                                else
                                    []
                               )
                        )
                        [ render child (key ++ "/el") ]

                Row children ->
                    Html.div [ style "display" "flex", style "gap" "16px" ]
                        (List.indexedMap renderIndex children)

                Col children ->
                    Html.div [ style "display" "flex", style "gap" "16px", style "flex-direction" "column" ]
                        (List.indexedMap renderIndex children)

                Button child ->
                    Html.button [] [ render child (key ++ "/button") ]

                LineInput onChange str ->
                    Html.input [ Html.Events.onInput onChange, Html.Attributes.value str ] []
    in
    Html.div []
        [ cssReset
        , render postElement "root"
        ]


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


lineInput : (String -> msg) -> String -> Element msg
lineInput onChange str =
    LineInput onChange str


style : String -> String -> Html.Attribute msg
style name value =
    Html.Attributes.style name value


cssReset : Html.Html msg
cssReset =
    Html.node "style" [] [ Html.text """
    
/* http://meyerweb.com/eric/tools/css/reset/ 
   v2.0 | 20110126
   License: none (public domain)
*/

html, body, div, span, applet, object, iframe,
h1, h2, h3, h4, h5, h6, p, blockquote, pre,
a, abbr, acronym, address, big, cite, code,
del, dfn, em, img, ins, kbd, q, s, samp,
small, strike, strong, sub, sup, tt, var,
b, u, i, center,
dl, dt, dd, ol, ul, li,
fieldset, form, label, legend,
table, caption, tbody, tfoot, thead, tr, th, td,
article, aside, canvas, details, embed, 
figure, figcaption, footer, header, hgroup, 
menu, nav, output, ruby, section, summary,
time, mark, audio, video {
  margin: 0;
  padding: 0;
  border: 0;
  font-size: 100%;
  font: inherit;
  vertical-align: baseline;
}
/* HTML5 display-role reset for older browsers */
article, aside, details, figcaption, figure, 
footer, header, hgroup, menu, nav, section {
  display: block;
}
body {
  line-height: 1;
}
ol, ul {
  list-style: none;
}
blockquote, q {
  quotes: none;
}
blockquote:before, blockquote:after,
q:before, q:after {
  content: '';
  content: none;
}
table {
  border-collapse: collapse;
  border-spacing: 0;
}


/* https://www.joshwcomeau.com/css/custom-css-reset/ */
/* 1. Use a more-intuitive box-sizing model */
*, *::before, *::after {
  box-sizing: border-box;
}

/* 2. Remove default margin */
* {
  margin: 0;
}

/* 3. Enable keyword animations */
@media (prefers-reduced-motion: no-preference) {
  html {
    interpolate-size: allow-keywords;
  }
}

body {
  /* 4. Add accessible line-height */
  line-height: 1.5;
  /* 5. Improve text rendering */
  -webkit-font-smoothing: antialiased;
}

/* 6. Improve media defaults */
img, picture, video, canvas, svg {
  display: block;
  max-width: 100%;
}

/* 7. Inherit fonts for form controls */
input, button, textarea, select {
  font: inherit;
}

/* 8. Avoid text overflows */
p, h1, h2, h3, h4, h5, h6 {
  overflow-wrap: break-word;
}

/* 9. Improve line wrapping */
p {
  text-wrap: pretty;
}
h1, h2, h3, h4, h5, h6 {
  text-wrap: balance;
}

/*
  10. Create a root stacking context
*/
#root, #__next {
  isolation: isolate;
}


button, input {
    border: none;
    background: none;
    color: inherit;
}

button {
    cursor: pointer;
}


* {
    outline: 0px solid white;
    transition: outline 250ms;
}
""" ]
