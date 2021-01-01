module Backup.Payloads exposing (..)

import Spotify.Payloads as Spotify


type alias Owner =
    { name : String
    , url : String
    }


type alias Track =
    Spotify.Track


type alias Playlist =
    { originalId : String
    , originalOwner : Owner
    , originalUrl : String
    , name : String
    , tracks : List Track
    }


spotifyToBackup : Spotify.Playlist -> List Spotify.Track -> Playlist
spotifyToBackup pl tracks =
    { originalId = pl.id
    , originalOwner = { name = Maybe.withDefault pl.owner.id pl.owner.displayName, url = pl.owner.url }
    , originalUrl = pl.url
    , name = pl.name
    , tracks = tracks
    }
