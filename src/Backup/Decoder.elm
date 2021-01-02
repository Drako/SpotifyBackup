module Backup.Decoder exposing (..)

import Backup.Payloads exposing (Owner, Playlist)
import Json.Decode exposing (Decoder, field, list, map2, map5, maybe, string)
import Spotify.Payloads exposing (Track)


track : Decoder Track
track =
    map5 Track
        (field "name" string)
        (field "album" string)
        (field "artists" <| list string)
        (field "url" <| maybe string)
        (field "uri" string)


owner : Decoder Owner
owner =
    map2 Owner
        (field "name" string)
        (field "url" string)


playlist : Decoder Playlist
playlist =
    map5 Playlist
        (field "originalId" string)
        (field "originalOwner" owner)
        (field "originalUrl" string)
        (field "name" string)
        (field "tracks" <| list track)


playlists : Decoder (List Playlist)
playlists =
    list playlist
