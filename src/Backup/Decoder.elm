module Backup.Decoder exposing (..)

import Backup.Payloads exposing (Owner, Playlist)
import Json.Decode exposing (Decoder, field, list, map2, map5, string)
import Spotify.Decoder exposing (track)


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
