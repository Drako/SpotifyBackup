module Backup.Encoder exposing (playlistToJson, playlistsToJson)

import Backup.Payloads exposing (Owner, Playlist, Track)
import Json.Encode exposing (Value, encode, list, object, string)


owner : Owner -> Value
owner it =
    object
        [ ( "name", string it.name )
        , ( "url", string it.url )
        ]


track : Track -> Value
track it =
    object
        [ ( "name", string it.name )
        , ( "album", string it.album )
        , ( "artists", list string it.artists )
        , ( "url", string it.url )
        , ( "uri", string it.uri )
        ]


playlist : Playlist -> Value
playlist it =
    object
        [ ( "originalId", string it.originalId )
        , ( "originalOwner", owner it.originalOwner )
        , ( "originalUrl", string it.originalUrl )
        , ( "name", string it.name )
        , ( "tracks", list track it.tracks )
        ]


playlistsToJson : List Playlist -> String
playlistsToJson pls =
    encode 0 <| list playlist pls


playlistToJson : Playlist -> String
playlistToJson pl =
    playlistsToJson [ pl ]
