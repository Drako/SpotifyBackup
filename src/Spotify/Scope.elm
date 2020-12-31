module Spotify.Scope exposing (..)


type Scope
    = PlaylistReadPrivate
    | PlaylistModifyPrivate
    | PlaylistModifyPublic


toString : Scope -> String
toString scope =
    case scope of
        PlaylistReadPrivate ->
            "playlist-read-private"

        PlaylistModifyPrivate ->
            "playlist-modify-private"

        PlaylistModifyPublic ->
            "playlist-modify-public"
