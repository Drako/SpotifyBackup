module Style exposing
    ( black
    , centeredBody
    , edges
    , headingRow
    , headingText
    , lightRed
    , spotifyBackground
    , spotifyButton
    , spotifyForeground
    , white
    )

import Element
    exposing
        ( Attribute
        , Color
        , Element
        , centerX
        , centerY
        , column
        , el
        , layout
        , mouseOver
        , padding
        , paddingEach
        , rgb255
        , row
        , spacing
        , text
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Utilities exposing (takeIf)


centeredBody : List (Element msg) -> Html msg
centeredBody elements =
    layout [] <|
        column [ centerX, centerY, spacing 15 ] elements


spotifyGreen : Color
spotifyGreen =
    rgb255 0x1D 0xB9 0x54


spotifyLightGreen : Color
spotifyLightGreen =
    rgb255 0x1E 0xD7 0x60


white : Color
white =
    rgb255 0xFF 0xFF 0xFF


black : Color
black =
    rgb255 0 0 0


lightRed : Color
lightRed =
    rgb255 0xFF 0xCC 0xCB


spotifyForeground : Color
spotifyForeground =
    rgb255 0xB3 0xB3 0xB3


spotifyBackground : Color
spotifyBackground =
    rgb255 0x28 0x28 0x28


edges : { top : Int, bottom : Int, left : Int, right : Int }
edges =
    { top = 0, bottom = 0, left = 0, right = 0 }


headingImpl : (List (Attribute msg) -> body -> Element msg) -> body -> Element msg
headingImpl elem body =
    elem
        [ Font.color white
        , Font.size 24
        , Border.widthEach { edges | bottom = 1 }
        , Border.solid
        , Border.color spotifyForeground
        , paddingEach { edges | bottom = 10, right = 10 }
        ]
        body


headingText : String -> Element msg
headingText txt =
    headingImpl el <|
        text txt


headingRow : List (Element msg) -> Element msg
headingRow elements =
    headingImpl row elements


spotifyButton : String -> Bool -> msg -> Element msg
spotifyButton label enabled action =
    let
        backgroundColor : Color
        backgroundColor =
            spotifyGreen |> takeIf enabled |> Maybe.withDefault spotifyBackground

        hoverEffect : List (Attribute msg)
        hoverEffect =
            [ mouseOver
                [ Border.color spotifyLightGreen
                , Background.color spotifyLightGreen
                ]
            ]
                |> takeIf enabled
                |> Maybe.withDefault []
    in
    el [ padding 10 ] <|
        button
            ([ Border.solid
             , Border.rounded 25
             , Border.color spotifyGreen
             , Border.width 1
             , padding 15
             , Background.color backgroundColor
             , Font.color white
             ]
                ++ hoverEffect
            )
            { onPress = action |> takeIf enabled
            , label = text label
            }
