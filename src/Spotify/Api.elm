module Spotify.Api exposing
    ( appendToPlaylist
    , createPlaylist
    , errorToString
    , fetchMorePlaylists
    , fetchMoreTracks
    , fetchPlaylists
    , fetchTracks
    , fetchUserId
    )

import Backup.Encoder exposing (trackUris)
import Http exposing (Error(..), Header, emptyBody, expectJson, expectWhatever, header, jsonBody, request, stringBody)
import Spotify.Decoder exposing (paging, playlist, playlistId, track, userId)
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


userPlaylistsUrl : String -> String
userPlaylistsUrl userId =
    spotifyUrl [ "users", userId, "playlists" ] []


playlistTracksUrl : String -> String
playlistTracksUrl playlistId =
    spotifyUrl [ "playlists", playlistId, "tracks" ] []


myUrl : String
myUrl =
    spotifyUrl [ "me" ] []


tracksUrl : Playlist -> String
tracksUrl playlist =
    spotifyUrl [ "playlists", playlist.id, "tracks" ]
        -- 100 is the maximum we can get with a single request
        [ int "limit" 100
        , int "offset" 0
        , string "fields" <|
            -- interestingly these fields are kinda ignored for local tracks
            "href,limit,next,offset,previous,total,"
                ++ "items(track(name,uri,album.name,artists(name),external_urls.spotify))"
        ]


authHeader : Token -> Header
authHeader { tokenType, accessToken } =
    header "Authorization" <| tokenType ++ " " ++ accessToken


contentTypeHeader : Header
contentTypeHeader =
    header "Content-Type" "application/json"


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


fetchUserId : Token -> (Result Error String -> msg) -> Cmd msg
fetchUserId tok msg =
    request
        { method = "GET"
        , headers = [ authHeader tok ]
        , url = myUrl
        , body = emptyBody
        , expect = expectJson msg userId
        , timeout = Nothing
        , tracker = Nothing
        }


createPlaylist : Token -> String -> String -> (Result Error String -> msg) -> Cmd msg
createPlaylist tok userId name msg =
    request
        { method = "POST"
        , headers = [ authHeader tok ]
        , url = userPlaylistsUrl userId
        , body = stringBody "application/json" <| "{\"name\": \"" ++ name ++ "\", \"public\": false}"
        , expect = expectJson msg playlistId
        , timeout = Nothing
        , tracker = Nothing
        }


appendToPlaylist : Token -> String -> List Track -> (Result Error () -> msg) -> Cmd msg
appendToPlaylist tok playlistId tracks msg =
    request
        { method = "POST"
        , headers = [ authHeader tok ]
        , url = playlistTracksUrl playlistId
        , body = jsonBody <| trackUris tracks
        , expect = expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }
