module Backup.PayloadTests exposing (..)

import Backup.Payloads exposing (spotifyToBackup)
import Backup.TestData exposing (..)
import Expect
import Spotify.Payloads as Spotify
import Test exposing (Test, test)


spotifyToBackupTest : Test
spotifyToBackupTest =
    test "spotifyToBackup" <|
        \_ ->
            let
                inputPlaylist : Spotify.Playlist
                inputPlaylist =
                    { href = ""
                    , url = "https://open.spotify.com/playlist/example"
                    , id = "<some-id>"
                    , images = []
                    , name = "example"
                    , isPublic = False
                    , tracks = { href = "", total = 2 }
                    , owner = { displayName = Just "Jon Doe", id = "", url = "https://open.spotify.com/user/example" }
                    }

                inputTracks : List Spotify.Track
                inputTracks =
                    [ trackObject, trackLocalFileObject ]
            in
            Expect.equal playlistObject <| spotifyToBackup inputPlaylist inputTracks
