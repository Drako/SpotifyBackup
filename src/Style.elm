module Style exposing (..)

import Element exposing (Color, Element, centerX, centerY, column, layout, mouseOver, padding, rgb255, spacing, text)
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


spotifyButton : String -> Maybe msg -> Element msg
spotifyButton label action =
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
