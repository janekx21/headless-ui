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
    }


type Msg
    = ChangeUsername String
    | ThingMsg Thing.Msg
    | ChangeChatMessage String
    | SendChatMessage
    | NoOp


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    { username = "", password = "", thingModel = Thing.init, chat = [], chatMessage = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ChangeUsername username ->
            { model | username = username }

        ThingMsg m ->
            { model | thingModel = Thing.update conf m model.thingModel }

        ChangeChatMessage s ->
            { model | chatMessage = s }

        SendChatMessage ->
            if model.chatMessage /= "" then
                { model | chat = model.chat ++ [ ( "Janek", model.chatMessage ) ], chatMessage = "" }

            else
                model


conf =
    { plugins =
        [ -- superRounder
          superTextRenderer

        --, basicLineInput
        , tabPlugin
        , basicButtons
        , basicLineInput
        , superRounder
        , debugSpace
        , tabbedExtra
        , i18n
            [ ( "Hello", "Hallo" )
            , ( "World", "Welt" )
            , ( "Please sign in", "Bitte einloggen" )
            , ( "Username", "Benutzername" )
            , ( "Password", "Passwort" )
            ]
        ]
    , intoMsg = ThingMsg
    }


view : Model -> Html.Html Msg
view model =
    toHtml
        conf
        model.thingModel
    <|
        tagged Tabs <|
            row
                [ col
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
                , text "Another Page"
                , col [ text "About us" ]
                ]


viewLogin model =
    col
        [ flexSpacer
        , row
            [ flexSpacer
            , el { defaultElAttributes | padding = 8, backgroundColor = "#c79cdc", rounding = 16 } <|
                col
                    [ row [ text "Please sign in ", fixedSpacer 8, text model.username ]
                    , row [ col [ flexSpacer, text "Username", flexSpacer ], lineInput ChangeUsername model.username ]
                    , row [ text "Password", lineInput ChangeUsername model.username ]
                    , button NoOp <| text "Login"
                    ]
            , flexSpacer
            ]
        , flexSpacer
        ]


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
                    if String.length txt > 50 then
                        row [ text <| String.slice 0 50 txt, text "..." ]

                    else if String.isEmpty txt then
                        el { defaultElAttributes | fontColor = "rgba(0,0,0,0.5)" } <| text "(empty)"

                    else
                        text txt

                _ ->
                    e
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
                    el { defaultElAttributes | padding = 1, backgroundColor = "#00458f", rounding = 24 } <|
                        el { defaultElAttributes | padding = 5, backgroundColor = "white", fontColor = "black", rounding = 24 } <|
                            LineInput msg value

                _ ->
                    e
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
                                                button (PluginM (String.fromInt i)) <|
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
    }
