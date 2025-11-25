module Basic exposing (..)

import Browser
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
            { model | thingModel = Thing.update m model.thingModel }

        ChangeChatMessage s ->
            { model | chatMessage = s }

        SendChatMessage ->
            if model.chatMessage /= "" then
                { model | chat = model.chat ++ [ ( "Janek", model.chatMessage ) ], chatMessage = "" }

            else
                model


view : Model -> Html.Html Msg
view model =
    toHtml
        { plugins = [ superRounder, superTextRenderer, basicButtons, basicLineInput, tabPlugin ]
        , intoMsg = ThingMsg
        }
        model.thingModel
    <|
        tagged Tabs <|
            row
                [ col
                    [ viewLogin model
                    , row
                        [ text "Hello"
                        , el { defaultElAttributes | fontColor = "red", backgroundColor = "black" } <| text "World"
                        , el { defaultElAttributes | padding = 20 } <| col [ text "A", text "B", button NoOp <| text "C", viewLogin model ]
                        , button NoOp <| col [ text "1", text "2", candy <| candy <| text "Hi" ]
                        , el { defaultElAttributes | backgroundColor = "red", padding = 8 } <|
                            el { defaultElAttributes | fontColor = "red", backgroundColor = "black", padding = 4 } <|
                                text "Janek"
                        ]
                    , viewChat model
                    ]
                , text "Hello world"
                , col [ text "Hello World" ]
                ]


viewLogin model =
    el { defaultElAttributes | padding = 8, backgroundColor = "#e6a3a2", rounding = 16 } <|
        col
            [ row [ text "Please sign in ", text model.username ]
            , row [ text "Username", lineInput ChangeUsername model.username ]
            , row [ text "Password", lineInput ChangeUsername model.username ]
            , button NoOp <| text "Login"
            ]


viewChat model =
    col
        [ text "Chat Window"
        , model.chat |> List.map (\( user, message ) -> row [ text user, text message ]) |> col
        , row [ lineInput ChangeChatMessage model.chatMessage, tagged Submit <| button SendChatMessage <| text "Send" ]
        ]


candy =
    el { defaultElAttributes | backgroundColor = "blue", padding = 2 }
        << el { defaultElAttributes | backgroundColor = "orange", padding = 2 }



-- User Plugins


{-| Renders empty text extra
-}
superTextRenderer : Plugin msg
superTextRenderer =
    { renderPoint =
        \e ->
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
    { renderPoint =
        \e ->
            case e of
                El attr (El attr2 child) ->
                    case attr.rounding of
                        0 ->
                            el { attr | rounding = attr2.rounding + attr.padding } <| el attr2 child

                        _ ->
                            e

                El attr child ->
                    case attr.rounding of
                        0 ->
                            el { attr | rounding = 16 } child

                        _ ->
                            e

                _ ->
                    e
    }


{-| Add basic button design.
Has extra style for submit buttons.
-}
basicButtons : Plugin msg
basicButtons =
    { renderPoint =
        \e ->
            case e of
                Button o child ->
                    Button o <|
                        el
                            { defaultElAttributes
                                | padding = 8
                                , backgroundColor = "#20252f"
                                , fontColor = "white"
                                , rounding = 24
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
                                , rounding = 24
                                , borderWidth = 2
                                , borderColor = "red"
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
    { renderPoint =
        \e ->
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
        \e ->
            let
                index =
                    0
            in
            case e of
                Tagged Tabs (Row children) ->
                    col
                        [ row <| List.indexedMap (\i c -> button (PluginM "New Index") <| text (String.fromInt i)) children
                        , List.Extra.getAt index children |> Maybe.withDefault None
                        ]

                _ ->
                    e
    , name = "Tab Plugin"
    }
