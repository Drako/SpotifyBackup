module Spotify.Api exposing (errorToString, fetchMorePlaylists, fetchMoreTracks, fetchPlaylists, fetchTracks)

import Http exposing (Error(..), Header, emptyBody, expectJson, header, request)
import Spotify.Decoder exposing (paging, playlist, track)
import Spotify.Payloads exposing (Paging, Playlist, Track)
import Spotify.Token exposing (Token)
import Url.Builder exposing (QueryParameter, crossOrigin, int, string)


spotifyUrl : List String -> List QueryParameter -> String
spotifyUrl =
    crossOrigin "https://api.spotify.com/v1"


myPlaylistsUrl : String
myPlaylistsUrl =
    -- 50 is the maximum we can get with a single request
    spotifyUrl [ "me", "playlists" ] [ int "limit" 50, int "offset" 0 ]


tracksUrl : Playlist -> String
tracksUrl playlist =
    spotifyUrl [ "playlists", playlist.id, "tracks" ]
        [ int "limit" 100
        , int "offset" 0
        , string "fields" <|
            "href,limit,next,offset,previous,total,"
                ++ "items(track(name,uri,album.name,artists(name),external_urls.spotify))"
        ]


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


fetchTracks : Token -> Playlist -> (Result Error (Paging Track) -> msg) -> Cmd msg
fetchTracks tok playlist msg =
    request
        { method = "GET"
        , headers = [ authHeader tok ]
        , url = tracksUrl playlist
        , body = emptyBody
        , expect = expectJson msg <| paging track
        , timeout = Nothing
        , tracker = Nothing
        }


fetchMoreTracks : Token -> Paging Track -> (Result Error (Paging Track) -> msg) -> Cmd msg
fetchMoreTracks tok current msg =
    case current.next of
        Just nextUrl ->
            request
                { method = "GET"
                , headers = [ authHeader tok ]
                , url = nextUrl
                , body = emptyBody
                , expect = expectJson msg <| paging track
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none
