module Backup.TestData exposing (..)

import Backup.Payloads exposing (Owner, Playlist, Track)


ownerJson : String
ownerJson =
    "{\"name\":\"Jon Doe\",\"url\":\"https://open.spotify.com/user/example\"}"


ownerObject : Owner
ownerObject =
    { url = "https://open.spotify.com/user/example", name = "Jon Doe" }


trackJson : String
trackJson =
    "{\"name\":\"Umbrella\",\"album\":\"Umbrella\",\"artists\":[\"Ember Island\"],"
        ++ "\"url\":\"https://open.spotify.com/track/2FX5HPP1FNNuLlt2UU9os7\","
        ++ "\"uri\":\"spotify:track:2FX5HPP1FNNuLlt2UU9os7\"}"


trackObject : Track
trackObject =
    { name = "Umbrella"
    , album = "Umbrella"
    , artists = [ "Ember Island" ]
    , url = Just "https://open.spotify.com/track/2FX5HPP1FNNuLlt2UU9os7"
    , uri = "spotify:track:2FX5HPP1FNNuLlt2UU9os7"
    }


trackLocalFileJson : String
trackLocalFileJson =
    "{\"name\":\"I See Stars\",\"album\":\"Lose Control\",\"artists\":[\"Wildstylez\"],"
        ++ "\"url\":null,"
        ++ "\"uri\":\"spotify:local:Wildstylez:Lose+Control:I+See+Stars:242\"}"


trackLocalFileObject : Track
trackLocalFileObject =
    { name = "I See Stars"
    , album = "Lose Control"
    , artists = [ "Wildstylez" ]
    , url = Nothing
    , uri = "spotify:local:Wildstylez:Lose+Control:I+See+Stars:242"
    }


playlistJson : String
playlistJson =
    "{\"originalId\":\"<some-id>\",\"originalOwner\":"
        ++ ownerJson
        ++ ",\"originalUrl\":\"https://open.spotify.com/playlist/example\",\"name\":\"example\","
        ++ "\"tracks\":["
        ++ trackJson
        ++ ","
        ++ trackLocalFileJson
        ++ "],\"totalTracks\":2}"


playlistObject : Playlist
playlistObject =
    { originalId = "<some-id>"
    , originalOwner = ownerObject
    , originalUrl = "https://open.spotify.com/playlist/example"
    , name = "example"
    , tracks = [ trackObject, trackLocalFileObject ]
    }


playlistsJson : String
playlistsJson =
    "[" ++ playlistJson ++ "]"


playlistsObject : List Playlist
playlistsObject =
    [ playlistObject ]


trackUrisJson : String
trackUrisJson =
    "[\"" ++ trackObject.uri ++ "\",\"" ++ trackLocalFileObject.uri ++ "\"]"


trackUrisObject : List Track
trackUrisObject =
    [ trackObject, trackLocalFileObject ]
