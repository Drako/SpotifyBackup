module Spotify.Api exposing (errorToString, fetchMorePlaylists, fetchPlaylists)

import Http exposing (Error(..), Header, emptyBody, expectJson, header, request)
import Spotify.Decoder exposing (paging, playlist)
import Spotify.Payloads exposing (Paging, Playlist)
import Spotify.Token exposing (Token)
import Url.Builder exposing (QueryParameter, crossOrigin, int)


spotifyUrl : List String -> List QueryParameter -> String
spotifyUrl =
    crossOrigin "https://api.spotify.com/v1"


myPlaylistsUrl : String
myPlaylistsUrl =
    -- 50 is the maximum we can get with a single request
    spotifyUrl [ "me", "playlists" ] [ int "limit" 50, int "offset" 0 ]


authHeader : Token -> Header
authHeader { tokenType, accessToken } =
    header "Authorization" <| tokenType ++ " " ++ accessToken


fetchPlaylists : Token -> (Result Error (Paging Playlist) -> msg) -> Cmd msg
fetchPlaylists tok msg =
    request
        { method = "GET"
        , headers = [ authHeader tok ]
        , url = myPlaylistsUrl
        , body = emptyBody
        , expect = expectJson msg <| paging playlist
        , timeout = Nothing
        , tracker = Nothing
        }


errorToString : Error -> String
errorToString err =
    case err of
        BadUrl url ->
            "Bad URL: " ++ url

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network error"

        BadStatus status ->
            "Bad HTTP status: " ++ String.fromInt status

        BadBody body ->
            "Bad response body: " ++ body


fetchMorePlaylists : Token -> Paging Playlist -> (Result Error (Paging Playlist) -> msg) -> Cmd msg
fetchMorePlaylists tok current msg =
    case current.next of
        Just nextUrl ->
            request
                { method = "GET"
                , headers = [ authHeader tok ]
                , url = nextUrl
                , body = emptyBody
                , expect = expectJson msg <| paging playlist
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none
