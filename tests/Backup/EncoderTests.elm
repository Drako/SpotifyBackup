module Backup.EncoderTests exposing (..)

import Backup.Encoder exposing (playlistToJson, playlistsToJson, trackUris)
import Backup.TestData exposing (..)
import Expect
import Json.Encode exposing (encode)
import Test exposing (Test, test)


playlistToJsonTest : Test
playlistToJsonTest =
    test "playlistToJson" <|
        -- even a single playlist is encoded as an array
        \_ -> Expect.equal playlistsJson <| playlistToJson playlistObject


playlistsToJsonTest : Test
playlistsToJsonTest =
    test "playlistsToJson" <|
        \_ -> Expect.equal playlistsJson <| playlistsToJson playlistsObject


trackUrisTest : Test
trackUrisTest =
    test "trackUris" <|
        \_ -> Expect.equal trackUrisJson <| encode 0 <| trackUris trackUrisObject
