module Basic exposing (..)

import Browser
import Dict exposing (Dict)
import Html
import List.Extra
import Thing exposing (..)


type alias Model =
    { username : String
    , password : String
    , thingModel : Thing.Model
    , chat : List ( String, String )
    , chatMessage : String
    , plugins : List (Plugin Msg)
    , deactivatedPlugins : List (Plugin Msg)
    , showPlugins : Bool
    }


type Msg
    = ChangeUsername String
    | ThingMsg Thing.Msg
    | ChangeChatMessage String
    | SendChatMessage
    | TogglePlugin (Plugin Msg)
    | ToggleShowPlugins
    | NoOp


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    { username = ""
    , password = ""
    , thingModel = Thing.init
    , chat = []
    , chatMessage = ""
    , plugins =
        [ superTextRenderer
        , headingPlugin
        , tabPlugin
        , basicButtons
        , basicLineInput
        , superRounder
        , tabbedExtra
        , i18n
            [ ( "Hello", "Hallo" )
            , ( "World", "Welt" )
            , ( "Please sign in", "Bitte einloggen" )
            , ( "Username", "Benutzername" )
            , ( "Password", "Passwort" )
            ]
        ]
    , deactivatedPlugins =
        [ debugSpace
        ]
    , showPlugins = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ChangeUsername username ->
            { model | username = username }

        ThingMsg m ->
            let
                c =
                    conf model.plugins
            in
            case c of
                Ok co ->
                    { model | thingModel = Thing.update co m model.thingModel }

                Err _ ->
                    model

        ChangeChatMessage s ->
            { model | chatMessage = s }

        SendChatMessage ->
            if model.chatMessage /= "" then
                { model | chat = model.chat ++ [ ( "Janek", model.chatMessage ) ], chatMessage = "" }

            else
                model

        TogglePlugin plugin ->
            if model.plugins |> List.member plugin then
                { model | plugins = model.plugins |> List.Extra.remove plugin, deactivatedPlugins = plugin :: model.deactivatedPlugins }

            else
                { model | deactivatedPlugins = model.deactivatedPlugins |> List.Extra.remove plugin, plugins = model.plugins ++ [ plugin ] }

        ToggleShowPlugins ->
            { model | showPlugins = not model.showPlugins }


conf p =
    initConfig p ThingMsg


view : Model -> Html.Html Msg
view model =
    let
        c =
            conf model.plugins
    in
    case c of
        Ok co ->
            toHtml co
                model.thingModel
            <|
                stack
                    [ tagged Tabs <|
                        row
                            [ col [ aboutUs ]
                            , col
                                [ text "Rect demo"
                                , row
                                    [ text "Hello"
                                    , fixedSpacer 64
                                    , el { defaultElAttributes | fontColor = "red", backgroundColor = "black" } <| text "World"
                                    , el { defaultElAttributes | padding = 20 } <| col [ text "A", text "B", button NoOp <| text "C" ]
                                    , flexSpacer
                                    , button NoOp <| col [ text "1", text "2", candy <| candy <| text "Hi" ]
                                    , el { defaultElAttributes | backgroundColor = "red", padding = 8 } <|
                                        el { defaultElAttributes | fontColor = "red", backgroundColor = "black", padding = 4 } <|
                                            text "Janek"
                                    ]
                                , Stack [ text "Hell", text "Wooooorld", el { defaultElAttributes | padding = 8 } <| text "!" ]
                                ]
                            , col [ text "Login Page", viewLogin model ]
                            , col [ text "Chat Example", viewChat model ]
                            , col [ text "Another Page" ]
                            ]
                    , viewPlugins model
                    ]

        Err error ->
            viewConfigError error


viewPlugins model =
    row
        [ flexSpacer
        , if model.showPlugins then
            col
                [ button ToggleShowPlugins <| text "> Hide"
                , fixedSpacer 16
                , col <|
                    List.intersperse (FixedSpacer 4)
                        (model.plugins |> List.map (\p -> button (TogglePlugin p) <| text p.name))
                , col <| List.intersperse (FixedSpacer 4) (model.deactivatedPlugins |> List.map (\p -> tagged Inactive <| button (TogglePlugin p) <| text p.name))
                ]

          else
            col
                [ button ToggleShowPlugins <| text "<"
                ]
        ]


viewLogin model =
    col
        [ flexSpacer
        , row
            [ flexSpacer
            , el { defaultElAttributes | padding = 8, backgroundColor = "#c79cdc", rounding = 16 } <|
                col <|
                    List.intersperse (FixedSpacer 8)
                        [ row [ text "Please sign in ", fixedSpacer 8, text model.username ]
                        , row [ verticalCenter <| text "Username", lineInput ChangeUsername model.username ]
                        , row [ verticalCenter <| text "Password", lineInput ChangeUsername model.username ]
                        , button NoOp <| text "Login"
                        ]
            , flexSpacer
            ]
        , flexSpacer
        ]


verticalCenter inner =
    col [ flexSpacer, inner, flexSpacer ]


viewChat model =
    row
        [ flexSpacer
        , col
            [ text "Centered Chat Window"
            , model.chat |> List.map (\( user, message ) -> row [ text user, fixedSpacer 8, text message ]) |> col
            , row [ lineInput ChangeChatMessage model.chatMessage, tagged Submit <| button SendChatMessage <| text "Send" ]
            ]
        , flexSpacer
        ]


candy =
    el { defaultElAttributes | backgroundColor = "blue", padding = 2 }
        << el { defaultElAttributes | backgroundColor = "orange", padding = 2 }


aboutUs =
    col
        [ headingParagraph "About Us (Level 1)" <|
            col
                [ text "The following text has the Main Title: The Art of Morning Routines"

                --, headingParagraph "" <|  text "Yes lets talks about something."
                , headingParagraph "Chapter 1: Understanding Your Natural Rhythm" <|
                    col
                        [ headingParagraph "The Science of Circadian Cycles" <|
                            col
                                [ headingParagraph "How Light Affects Your Wake-Up Time" <|
                                    col
                                        [ text "Your body's internal clock responds powerfully to light exposure. When sunlight enters your eyes in the morning, it triggers a cascade of hormonal responses that help you feel alert and energized. This natural process has been fine-tuned over millions of years of human evolution."
                                        ]
                                , headingParagraph "The Role of Cortisol in Morning Energy" <|
                                    col
                                        [ text "Cortisol, often called the stress hormone, actually plays a beneficial role in your morning routine. It naturally peaks about 30 minutes after waking, helping you transition from sleep to active consciousness. Understanding this rhythm can help you time your activities more effectively."
                                        ]
                                , headingParagraph "Creating Consistency in Your Schedule" <|
                                    col
                                        [ text "Building a consistent wake-up time helps stabilize your circadian rhythm. Even on weekends, maintaining similar sleep and wake times can improve your overall energy levels and mood throughout the week."
                                        ]
                                ]
                        ]
                , headingParagraph "Chapter 2: Practical Morning Habits" <|
                    col
                        [ headingParagraph "Movement and Exercise" <|
                            col
                                [ headingParagraph "Gentle Stretching Techniques" <| text "Starting with simple stretches can awaken your muscles and increase blood flow. Focus on major muscle groups like your back, legs, and shoulders to release overnight tension."
                                , headingParagraph "The Benefits of Morning Walks" <| text "A brief walk outdoors combines light exposure, gentle exercise, and fresh air—three powerful elements that can set a positive tone for your entire day."
                                ]
                        ]
                ]
        ]


headingParagraph heading body =
    col
        [ tagged Heading <| text heading
        , body
        ]



--User Plugins


{-| Renders empty text extra
-}
superTextRenderer : Plugin msg
superTextRenderer =
    { name = "Super Text Renderer"
    , init = Dict.empty
    , update = \_ _ -> Dict.empty
    , renderPoint =
        \_ e ->
            case e of
                Text txt ->
                    if String.length txt > 50 && String.length txt < 100 then
                        row [ text <| String.slice 0 50 txt, text "..." ]

                    else if String.isEmpty txt then
                        el { defaultElAttributes | fontColor = "rgba(0,0,0,0.5)" } <| text "(empty)"

                    else
                        text txt

                _ ->
                    e
    , dependencies = []
    }


{-| Rounds everything
-}
superRounder : Plugin msg
superRounder =
    { name = "Super Rounder"
    , init = Dict.empty
    , update = \_ _ -> Dict.empty
    , renderPoint =
        \_ e ->
            case e of
                El attr (El attr2 child) ->
                    case attr.rounding of
                        0 ->
                            el { attr | rounding = attr2.rounding + attr.padding } <| el attr2 child

                        _ ->
                            e

                El attr child ->
                    if attr.rounding < 16 then
                        el { attr | rounding = 16 } child

                    else
                        e

                _ ->
                    e
    , dependencies = []
    }


{-| Add basic button design.
Has extra style for submit buttons.
-}
basicButtons : Plugin msg
basicButtons =
    { name = "Basic Buttons"
    , init = Dict.empty
    , update = \_ _ -> Dict.empty
    , renderPoint =
        \_ e ->
            case e of
                Button o child ->
                    Button o <|
                        el
                            { defaultElAttributes
                                | padding = 8
                                , backgroundColor = "#20252f"
                                , fontColor = "white"
                                , rounding = 8
                            }
                        <|
                            child

                Tagged Inactive (Button o (El _ child)) ->
                    Button o <|
                        el
                            { defaultElAttributes
                                | padding = 8
                                , backgroundColor = "#00458f"
                                , fontColor = "gray"
                                , rounding = 8
                                , borderWidth = 2
                                , borderColor = "gray"
                            }
                        <|
                            child

                Tagged Active (Button o (El _ child)) ->
                    Button o <|
                        el
                            { defaultElAttributes
                                | padding = 8
                                , backgroundColor = "#008f45"
                                , fontColor = "white"
                                , rounding = 8
                                , borderWidth = 2
                                , borderColor = "#20af65"
                            }
                        <|
                            child

                Tagged Submit (Button o (El _ child)) ->
                    Button o <|
                        el
                            { defaultElAttributes
                                | padding = 8
                                , backgroundColor = "#00458f"
                                , fontColor = "white"
                                , rounding = 8
                                , borderWidth = 2
                                , borderColor = "#2065af"
                            }
                        <|
                            child

                _ ->
                    e
    , dependencies = []
    }


{-| Add basic line input design
-}
basicLineInput : Plugin msg
basicLineInput =
    { name = "Basic Line Input"
    , init = Dict.empty
    , update = \_ _ -> Dict.empty
    , renderPoint =
        \_ e ->
            case e of
                LineInput msg value ->
                    el
                        { defaultElAttributes
                            | padding = 8
                            , rounding = 24
                            , borderWidth = 1
                            , borderColor = "#00458f"
                        }
                    <|
                        LineInput msg value

                _ ->
                    e
    , dependencies = []
    }


{-| Add tabs
-}
tabPlugin : Plugin msg
tabPlugin =
    { renderPoint =
        \m e ->
            let
                index : Int
                index =
                    m
                        |> Dict.get "index"
                        |> Maybe.andThen String.toInt
                        |> Maybe.withDefault 0

                findText : Element x -> Maybe String
                findText el =
                    case el of
                        Text txt ->
                            Just txt

                        None ->
                            Nothing

                        El _ child ->
                            findText child

                        Row children ->
                            children |> List.filterMap findText |> List.head

                        Col children ->
                            children |> List.filterMap findText |> List.head

                        Button _ child ->
                            findText child

                        LineInput _ str ->
                            Just str

                        Tagged _ child ->
                            findText child

                        FlexSpacer ->
                            Nothing

                        FixedSpacer _ ->
                            Nothing

                        Stack children ->
                            children |> List.filterMap findText |> List.head
            in
            case e of
                Tagged Tabs (Row children) ->
                    tagged Tabs <|
                        col
                            [ row <|
                                List.intersperse (FixedSpacer 8) <|
                                    List.indexedMap
                                        (\i c ->
                                            tagged
                                                (if i == index then
                                                    Active

                                                 else
                                                    Inactive
                                                )
                                            <|
                                                button (pluginEvent (String.fromInt i)) <|
                                                    text (findText c |> Maybe.withDefault (String.fromInt i))
                                        )
                                        children
                            , List.Extra.getAt index children |> Maybe.withDefault None
                            ]

                _ ->
                    e
    , name = "Tab Plugin"
    , init = Dict.empty |> Dict.insert "index" "0"
    , update =
        \msg model ->
            case String.toInt msg of
                Just i ->
                    model |> Dict.insert "index" (String.fromInt i)

                Nothing ->
                    model
    , dependencies = []
    }


{-| Rounds everything
-}
debugSpace : Plugin msg
debugSpace =
    { name = "Debug Space"
    , init = Dict.empty
    , update = \_ _ -> Dict.empty
    , renderPoint =
        \_ e ->
            case e of
                FlexSpacer ->
                    stack
                        [ FlexSpacer
                        , el { defaultElAttributes | borderColor = "blue", borderWidth = 2, borderStyle = "dashed", fontColor = "blue", fontSize = 12 } <| None
                        ]

                FixedSpacer px ->
                    stack
                        [ FixedSpacer px
                        , el { defaultElAttributes | borderColor = "green", borderWidth = 2, borderStyle = "dashed", fontColor = "green", fontSize = 12 } <|
                            if px > 48 then
                                text <| String.fromInt px ++ "px"

                            else
                                None
                        ]

                El attr child ->
                    if attr.rounding < 16 then
                        el { attr | rounding = 16 } child

                    else
                        e

                _ ->
                    e
    , dependencies = []
    }


{-| Translates everything
-}
i18n : List ( String, String ) -> Plugin msg
i18n dict =
    { name = "i18n"
    , init = Dict.empty
    , update = \_ _ -> Dict.empty
    , renderPoint =
        \_ e ->
            case e of
                Text txt ->
                    text <| List.foldl (\( k, v ) acc -> String.replace k v acc) txt dict

                _ ->
                    e
    , dependencies = []
    }


{-| Translates everything
-}
tabbedExtra : Plugin msg
tabbedExtra =
    { name = "Tabbed Extra"
    , init = Dict.empty
    , update = \_ _ -> Dict.empty
    , renderPoint =
        \_ e ->
            case e of
                -- Rotates the nav bar vertical
                -- From A B C
                -- To A
                --    B
                --    C
                Tagged Tabs (Col [ Row navItems, page ]) ->
                    tagged Tabs <| row [ col navItems, page ]

                _ ->
                    e
    , dependencies = [ tabPlugin.name ]
    }


{-| Adds heading levels
-}
headingPlugin : Plugin msg
headingPlugin =
    { name = "Heading Plugin"
    , init = Dict.empty
    , update = \_ _ -> Dict.empty
    , renderPoint =
        \_ e ->
            let
                countHeading : Element x -> Int
                countHeading el =
                    case el of
                        Col ((Tagged Heading _) :: rest) ->
                            (rest |> List.map countHeading |> List.maximum |> Maybe.withDefault 0) + 1

                        Text _ ->
                            0

                        None ->
                            0

                        El _ child ->
                            countHeading child

                        Row children ->
                            children |> List.map countHeading |> List.maximum |> Maybe.withDefault 0

                        Col children ->
                            children |> List.map countHeading |> List.maximum |> Maybe.withDefault 0

                        Button _ child ->
                            countHeading child

                        LineInput _ _ ->
                            0

                        Tagged _ child ->
                            countHeading child

                        FlexSpacer ->
                            0

                        FixedSpacer _ ->
                            0

                        Stack children ->
                            children |> List.map countHeading |> List.maximum |> Maybe.withDefault 0
            in
            case e of
                Col [ Tagged Heading (Text headingLabel), body ] ->
                    let
                        level =
                            4 - countHeading body
                    in
                    col
                        [ tagged Heading <| el { defaultElAttributes | fontSize = 32 - level * 4 } <| text headingLabel

                        --, text <| String.fromInt level
                        , fixedSpacer <| 48 - level * 8

                        --, el { defaultElAttributes | padding = 16 } <|
                        , body
                        ]

                _ ->
                    e
    , dependencies = []
    }
