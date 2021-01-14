module Pages.Home exposing (..)

import Browser exposing (UrlRequest(..))
import Element exposing (centerX, newTabLink, text)
import Element.Font as Font
import Html exposing (Html)
import Spotify.Scope as Scope
import Style exposing (centeredBody, spotifyButton, spotifyForeground)
import Url


type HomeMsg
    = Authenticate UrlRequest


redirectToExternal : String -> HomeMsg
redirectToExternal urlString =
    Authenticate <| External urlString


view : Html HomeMsg
view =
    centeredBody
        [ spotifyButton "Login to Spotify." True <|
            redirectToExternal
                ("https://accounts.spotify.com/authorize"
                    ++ "?client_id=d09a22d09df145d4bd86d541fd46f4b4"
                    ++ "&response_type=token"
                    ++ "&redirect_uri="
                    ++ Url.percentEncode "https://spotify-backup.drako.guru/callback"
                    ++ "&show_dialog=true"
                    ++ "&scope="
                    ++ Scope.toQueryParams Scope.all
                )
        , newTabLink [ Font.underline, Font.color spotifyForeground, Font.size 14, centerX ]
            { url = "https://github.com/Drako/SpotifyBackup"
            , label = text "Source on GitHub"
            }
        ]
