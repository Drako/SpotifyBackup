module Pages.Home exposing (..)

import Browser exposing (UrlRequest(..))
import Html exposing (Html)
import Spotify.Scope as Scope
import Style exposing (centeredBody, spotifyButton)
import Url


type HomeMsg
    = Authenticate UrlRequest


redirectToExternal : String -> HomeMsg
redirectToExternal urlString =
    Authenticate <| External urlString


view : Html HomeMsg
view =
    centeredBody
        [ spotifyButton "Login to Spotify." <|
            Just
                (redirectToExternal
                    ("https://accounts.spotify.com/authorize"
                        ++ "?client_id=d09a22d09df145d4bd86d541fd46f4b4"
                        ++ "&response_type=token"
                        ++ "&redirect_uri="
                        ++ Url.percentEncode "https://spotify-backup.drako.guru/callback"
                        ++ "&show_dialog=true"
                        ++ "&scope="
                        ++ Scope.toQueryParams Scope.all
                    )
                )
        ]