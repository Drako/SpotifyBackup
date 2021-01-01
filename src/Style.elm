module Style exposing (..)

import Element
    exposing
        ( Color
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


spotifyForeground : Color
spotifyForeground =
    rgb255 0xB3 0xB3 0xB3


spotifyBackground : Color
spotifyBackground =
    rgb255 0x28 0x28 0x28


edges : { top : Int, bottom : Int, left : Int, right : Int }
edges =
    { top = 0, bottom = 0, left = 0, right = 0 }


heading : String -> Element msg
heading txt =
    el
        [ Font.color white
        , Font.size 24
        , Border.widthEach { edges | bottom = 1 }
        , Border.solid
        , Border.color spotifyForeground
        , paddingEach { edges | bottom = 10, right = 10 }
        ]
    <|
        text txt


headingRow : List (Element msg) -> Element msg
headingRow elements =
    row
        [ Font.color white
        , Font.size 24
        , Border.widthEach { edges | bottom = 1 }
        , Border.solid
        , Border.color spotifyForeground
        , paddingEach { edges | bottom = 10, right = 10 }
        ]
        elements


spotifyButton : String -> Maybe msg -> Element msg
spotifyButton label action =
    el [ padding 10 ] <|
        button
            [ Border.solid
            , Border.rounded 25
            , Border.color spotifyGreen
            , Border.width 1
            , padding 15
            , Background.color spotifyGreen
            , Font.color white
            , mouseOver
                [ Border.color spotifyLightGreen
                , Background.color spotifyLightGreen
                ]
            ]
            { onPress = action
            , label = text label
            }
