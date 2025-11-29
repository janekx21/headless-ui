module Thing exposing
    ( Element(..)
    , text
    , row
    , col
    , stack
    , el
    , defaultElAttributes
    , fixedSpacer
    , flexSpacer
    , button
    , lineInput
    , tagged
    , Tag(..)
    , toHtml
    , init
    , Model
    , Msg(..)
    , update
    , Plugin
    , RenderPoint
    , pluginEvent
    , initHtmlConfig, viewConfigError
    )

{-| This framework is the implementation and interface for an abstract UI.
The framework can be extended with plugins.

    view : Model -> Html.Html Msg
    view model =
        toHtml
            { plugins =
                [-- Place you plugins here
                ]
            , intoMsg = ThingMsg
            }
            model.thingModel
        <|
            row [ text "Here is some text in a row" ]


# Element Definition

Elements are the nodes of this framework. They are the building blocks.

There are the following type of elements:

  - None is no element at all. It will not be rendered at all.
  - Text is just a label in the UI
  - El is for styling
  - Row & Col are rows and columns
  - Button is a simple interactable that you can click
  - LineInput is a single line text input
  - Tagged is a node that has a special tag. Tags indicate element kinds.
  - FlexSpacer is space that can grow
  - FixedSpacer is space that has a fixed size
  - Stack is a stack of elements on top of each other

@docs Element


## The following functions construct the elements.

@docs text

@docs row
@docs col
@docs stack

@docs el

`el` has a const set of default attributes.

@docs defaultElAttributes

@docs fixedSpacer
@docs flexSpacer

@docs button
@docs lineInput

@docs tagged
@docs Tag

@docs toHtml


# Setup

@docs init
@docs Model
@docs Msg
@docs update


# Plugins

@docs Plugin
@docs RenderPoint
@docs pluginEvent

-}

import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events


{-| Represents an abstract UI element. The type is completely exposed for modification purposes in the `Plugin` system.
-}
type Element msg
    = None
    | Text String
    | El ElAttributes (Element msg)
    | Row (List (Element msg))
    | Col (List (Element msg))
    | Button msg (Element msg)
    | LineInput (String -> msg) String
    | Tagged Tag (Element msg)
    | FlexSpacer
    | FixedSpacer Int
    | Stack (List (Element msg))



-- TODO --| Tabbed (List ( Element msg, Element msg ))


type alias ElAttributes =
    { fontColor : String
    , fontSize : Int
    , backgroundColor : String
    , backgroundImage : String
    , padding : Int
    , rounding : Int
    , borderColor : String
    , borderWidth : Int
    , borderStyle : String
    }


{-| The default attributes of an el element. Default color, font size, background etc.
-}
defaultElAttributes : ElAttributes
defaultElAttributes =
    { fontColor = "black"
    , fontSize = 16
    , backgroundColor = "transparent"
    , backgroundImage = "none"
    , padding = 0
    , rounding = 0
    , borderColor = "black"
    , borderWidth = 0
    , borderStyle = "solid"
    }


{-| A tag that you can place around your components to signal a use case
-}
type Tag
    = Submit
    | Cancle
    | Active
    | Inactive
    | Disabled
      --
    | Heading
      -- inspired by shadcn components
      -- https://ui.shadcn.com/docs/components
    | Accordion
    | Alert
    | Dialog
    | Avatar
    | Badge
    | Breadcrumb
    | ButtonGroup
    | Calendar
    | Card
    | Carousel
    | Chart
      -- Like a dropdown
    | Combobox
      -- Like a context menu but without opening
    | Command
    | ContextMenu
    | Collapsible
    | Table
    | DatePicker
    | Drawer
    | DropdownMenu
    | Empty
    | Field
    | InputGroup
    | Form
    | HoverCard
    | Item
    | KeyboardButton
      -- For fields
    | Label
    | Menubar
    | NavigationMenu
    | Pagination
    | Popover
    | Progress
    | RadioGroup
    | Resizable
    | ScrollArea
    | Select
    | Seperator
      -- Dialog on the side
    | Sheet
    | Sidebar
    | Skeleton
    | Slider
    | Sonner
    | Spinner
    | Switch
    | Tabs
    | Textarea
    | Toast
    | ToggleGroup
    | Toggle
    | Tooltip


type alias HtmlConfig msg =
    { plugins : List (Plugin msg)
    , intoMsg : Msg -> msg
    }


initHtmlConfig : List (Plugin msg) -> (Msg -> msg) -> Result ConfigError (HtmlConfig msg)
initHtmlConfig plugins intoMsg =
    let
        missingDependencies =
            plugins
                |> List.concatMap
                    (\plugin ->
                        plugin.dependencies
                            |> List.filter
                                (\dependencyName ->
                                    not (plugins |> List.any (\otherPlugin -> otherPlugin.name == dependencyName))
                                )
                    )
    in
    if missingDependencies |> List.isEmpty then
        Ok <|
            { plugins = plugins
            , intoMsg = intoMsg
            }

    else
        Err <| DependenciesMissing missingDependencies


type ConfigError
    = DependenciesMissing (List String)


viewConfigError : ConfigError -> Html.Html msg
viewConfigError err =
    case err of
        DependenciesMissing missing ->
            Html.div []
                [ Html.text "Missing dependencies"
                , Html.ul [] (missing |> List.map (\item -> Html.li [] [ Html.text item ]))
                ]


{-| A Plugin can add functionality to your application. They are developed independent of one another.
-}
type alias Plugin msg =
    { renderPoint : RenderPoint msg
    , name : String
    , init : PluginModel
    , update : String -> PluginModel -> PluginModel
    , dependencies : List String
    }


{-| The state of a single plugin is a key value store
-}
type alias PluginModel =
    Dict String String


{-| The extension point/hot point of a plugin for rendering
-}
type alias RenderPoint msg =
    PluginModel -> Element (PluginMsg msg) -> Element (PluginMsg msg)


type PluginMsg msg
    = PluginM String
    | External msg


{-| Create a plugin event. Use this inside plugins to send a message to the own plugin.
-}
pluginEvent : String -> PluginMsg msg
pluginEvent str =
    PluginM str


{-| This is the internal state of the framework
-}
type alias Model =
    { hovering : Dict String Bool
    , pluginModels : Dict String (Dict String String)
    }


{-| This is the internal message of the framework
-}
type Msg
    = NoOp
    | Hover String
    | UnHover String
    | PluginEvent String String


{-| Initializes the internal framework state
-}
init : Model
init =
    { hovering = Dict.empty, pluginModels = Dict.empty }


{-| This is the internal update function
-}
update : HtmlConfig msg -> Msg -> Model -> Model
update conf msg model =
    case msg of
        NoOp ->
            model

        Hover key ->
            { model | hovering = Dict.insert key True model.hovering }

        UnHover key ->
            { model | hovering = Dict.remove key model.hovering }

        PluginEvent pluginName eventName ->
            -- TODO do stuff
            let
                u =
                    conf.plugins |> List.filter (\p -> p.name == pluginName) |> List.head

                -- TODO this could be nicer build
                m =
                    model.pluginModels |> Dict.get pluginName |> Maybe.withDefault (u |> Maybe.map .init |> Maybe.withDefault Dict.empty)
            in
            case ( u, m ) of
                ( Just plugin, pluginModel ) ->
                    { model | pluginModels = model.pluginModels |> Dict.insert pluginName (plugin.update eventName pluginModel) }

                -- Plugin not found?
                _ ->
                    model


mapMsg : (a -> b) -> Element a -> Element b
mapMsg func element =
    case element of
        None ->
            None

        Text txt ->
            Text txt

        El attr child ->
            El attr (mapMsg func child)

        Row children ->
            Row (List.map (mapMsg func) children)

        Col children ->
            Col (List.map (mapMsg func) children)

        Button onClick child ->
            Button (func onClick) (mapMsg func child)

        LineInput onChange str ->
            LineInput (func << onChange) str

        Tagged tag child ->
            Tagged tag (mapMsg func child)

        FlexSpacer ->
            FlexSpacer

        FixedSpacer px ->
            FixedSpacer px

        Stack children ->
            Stack (List.map (mapMsg func) children)


{-| The main rendering function you will use
-}
toHtml : HtmlConfig msg -> Model -> Element msg -> Html.Html msg
toHtml config model preElement =
    let
        postElement : Element msg
        postElement =
            config.plugins
                |> List.foldl
                    (\item acc ->
                        preProcess
                            (\og ->
                                item.renderPoint (model.pluginModels |> Dict.get item.name |> Maybe.withDefault item.init) (mapMsg External og)
                                    |> mapMsg
                                        (\x ->
                                            case x of
                                                External m ->
                                                    m

                                                PluginM m ->
                                                    config.intoMsg (PluginEvent item.name m)
                                        )
                            )
                            acc
                    )
                    preElement

        --(mapMsg (\x -> External x) preElement)
        preProcess : (Element a -> Element a) -> Element a -> Element a
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

                    Button onClick child ->
                        Button onClick (preProcess func child)

                    LineInput onChange str ->
                        LineInput onChange str

                    Tagged tag child ->
                        Tagged tag (preProcess func child)

                    FlexSpacer ->
                        FlexSpacer

                    FixedSpacer px ->
                        FixedSpacer px

                    Stack children ->
                        Stack (children |> List.map (preProcess func))

        render : Element msg -> String -> Html.Html msg
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
                         , style "font-size" (String.fromInt attr.fontSize ++ "px")
                         , style "background-color" attr.backgroundColor
                         , style "background-image" attr.backgroundImage
                         , style "padding" (String.fromInt attr.padding ++ "px")
                         , style "border-radius" (String.fromInt attr.rounding ++ "px")
                         , style "border-color" attr.borderColor
                         , style "border-width" (String.fromInt attr.borderWidth ++ "px")
                         , style "border-style" attr.borderStyle
                         , class "el"
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
                    Html.div [ class "row" ]
                        (List.indexedMap renderIndex children)

                Col children ->
                    Html.div [ class "col" ]
                        (List.indexedMap renderIndex children)

                Button onClick child ->
                    Html.button [ Html.Events.onClick onClick ] [ render child (key ++ "/button") ]

                LineInput onChange str ->
                    Html.input [ Html.Events.onInput onChange, Html.Attributes.value str, style "flex-grow" "1" ] []

                -- Tags are not rendered
                Tagged _ child ->
                    render child (key ++ "/tagged")

                FlexSpacer ->
                    Html.div [ class "flex-spacer" ] []

                FixedSpacer px ->
                    Html.div [ style "width" <| String.fromInt px ++ "px", style "height" <| String.fromInt px ++ "px" ] []

                Stack children ->
                    Html.div [ class "stack" ] (List.indexedMap renderIndex children)
    in
    Html.div [ style "width" "100vw", style "height" "100vh", style "display" "flex", style "flex-direction" "column" ]
        [ cssReset
        , render postElement "root"
        ]


{-| An element that can style your application
-}
el : ElAttributes -> Element msg -> Element msg
el attr child =
    El attr child


{-| Some text
-}
text : String -> Element msg
text str =
    Text str


{-| A row of item
-}
row : List (Element msg) -> Element msg
row children =
    Row children


{-| A column of items
-}
col : List (Element msg) -> Element msg
col children =
    Col children


{-| An interactable that you can click on
-}
button : msg -> Element msg -> Element msg
button onClick child =
    Button onClick child


{-| An interactable single line of text
-}
lineInput : (String -> msg) -> String -> Element msg
lineInput onChange str =
    LineInput onChange str


{-| Add a tag to the inner element
-}
tagged : Tag -> Element msg -> Element msg
tagged tag =
    Tagged tag


{-| Some space that will dynamically grow
-}
flexSpacer : Element msg
flexSpacer =
    FlexSpacer


{-| Some fixed size space in px
-}
fixedSpacer : Int -> Element msg
fixedSpacer px =
    FixedSpacer px


{-| Some elements that are stacked on top of each other
-}
stack : List (Element msg) -> Element msg
stack children =
    Stack children


style : String -> String -> Html.Attribute msg
style name value =
    Html.Attributes.style name value


class : String -> Html.Attribute msg
class name =
    Html.Attributes.class name


css : String
css =
    """
.row,.col,.el,.stack {
    display: flex;
}
.col {
    flex-direction: column;
    height: 100%;
}
.row>.col:has(.flex-spacer) {
   flex-grow: 1;
}
--.col>.row:has(>.flex-spacer) {
--   flex-grow: 1;
--}
.stack {
    display: grid;
}
.stack>* {
    grid-row: 1;
    grid-column: 1;
}
.stack:has(.flex-spacer) {
    flex-grow: 1;
}
.flex-spacer {
    flex-grow: 1;
}
.flex-spacer,.row,.col,.stack {
    pointer-events: none;
}
button,input {
    pointer-events: all;
}
"""


cssReset : Html.Html msg
cssReset =
    Html.node "style" [] [ Html.text <| """
    
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
    padding: 0;
}

* {
    outline: 0px solid white;
    transition: outline 250ms;

}


""" ++ css ]
