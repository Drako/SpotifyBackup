module Backup.DecoderTests exposing (..)

import Backup.Decoder exposing (owner, playlist, playlists, track)
import Backup.TestData exposing (..)
import Expect
import Json.Decode exposing (decodeString)
import Test exposing (Test, test)


ownerTest : Test
ownerTest =
    test "owner" <|
        \_ ->
            Expect.equal (Ok ownerObject) <| decodeString owner ownerJson


trackTest : Test
trackTest =
    test "track" <|
        \_ ->
            Expect.equal (Ok trackObject) <| decodeString track trackJson


trackLocalFileTest : Test
trackLocalFileTest =
    test "track without URL" <|
        \_ ->
            Expect.equal (Ok trackLocalFileObject) <| decodeString track trackLocalFileJson


playlistTest : Test
playlistTest =
    test "playlist" <|
        \_ -> Expect.equal (Ok playlistObject) <| decodeString playlist playlistJson


playlistsTest : Test
playlistsTest =
    test "playlists" <|
        \_ -> Expect.equal (Ok playlistsObject) <| decodeString playlists playlistsJson
