module Spotify.Scope exposing (..)


type Scope
    = PlaylistReadPrivate
    | PlaylistModifyPrivate
    | PlaylistModifyPublic
    | PlaylistReadCollaborative


all : List Scope
all =
    [ PlaylistReadPrivate, PlaylistModifyPrivate, PlaylistModifyPublic, PlaylistReadCollaborative ]


toString : Scope -> String
toString scope =
    case scope of
        PlaylistReadPrivate ->
            "playlist-read-private"

        PlaylistModifyPrivate ->
            "playlist-modify-private"

        PlaylistModifyPublic ->
            "playlist-modify-public"

        PlaylistReadCollaborative ->
            "playlist-read-collaborative"
