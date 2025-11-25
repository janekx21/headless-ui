module Chat exposing (..)

import Browser
import Html
,,,
import Thing exposing (..)


type alias Model =
    { username : String
    , password : String
    , thingModel : Thing.Model
    }


type Msg
    = ChangeUsername String
    | ThingMsg Thing.Msg


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init : Model
init =
    { username = "", password = "", thingModel = Thing.init }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeUsername username ->
            { model | username = username }

        ThingMsg m ->
            { model | thingModel = Thing.update m model.thingModel }


view : Model -> Html.Html Msg
view model =
    toHtml
        { plugins = [ superRounder, superTextRenderer, basicButtons, basicLineInput ]
        , intoMsg = ThingMsg
        }
        model.thingModel
    <|
        col
            [ viewLogin model
            , row
                [ text "Hello"
                , el { defaultElAttributes | fontColor = "red", backgroundColor = "black" } <| text "World"
                , el { defaultElAttributes | padding = 20 } <| col [ text "A", text "B", button <| text "C", viewLogin model ]
                , button <| col [ text "1", text "2", candy <| candy <| text "Hi" ]
                , el { defaultElAttributes | backgroundColor = "red", padding = 8 } <|
                    el { defaultElAttributes | fontColor = "red", backgroundColor = "black", padding = 4 } <|
                        text "Janek"
                ]
            ]


viewLogin model =
    el { defaultElAttributes | padding = 8, backgroundColor = "#e6a3a2", rounding = 16 } <|
        col
            [ row [ text "Please sign in ", text model.username ]
            , row [ text "Username", lineInput ChangeUsername model.username ]
            , row [ text "Password", lineInput ChangeUsername model.username ]
            , button <| text "Login"
            ]


candy =
    el { defaultElAttributes | backgroundColor = "blue", padding = 2 }
        << el { defaultElAttributes | backgroundColor = "orange", padding = 2 }



-- User Plugins


superTextRenderer : Plugin msg
superTextRenderer =
    { renderPoint =
        \e ->
            case e of
                Text txt ->
                    if String.length txt > 3 then
                        row [ text txt, text "..." ]

                    else
                        row [ text "(", text txt, text ")" ]

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
                El attr child ->
                    case attr.rounding of
                        0 ->
                            el { attr | rounding = 16 } child

                        _ ->
                            e

                _ ->
                    e
    }


{-| Add basic button design
-}
basicButtons : Plugin msg
basicButtons =
    { renderPoint =
        \e ->
            case e of
                Button child ->
                    Button <| el { defaultElAttributes | padding = 8, backgroundColor = "#00458f", fontColor = "white", rounding = 24 } <| child

                _ ->
                    e
    }


{-| Add basic button design
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
