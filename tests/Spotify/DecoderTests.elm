module Spotify.DecoderTests exposing (..)

import Expect
import Json.Decode exposing (decodeString)
import Spotify.Decoder exposing (image, paging, playlist, playlistId, spotifyUrl, track, userId)
import Spotify.Payloads exposing (Image, Track)
import Spotify.TestData exposing (..)
import Test exposing (Test, test)
import TestHelpers exposing (parameterizedWithTitles)


userIdTest : Test
userIdTest =
    test "userId" <|
        \_ -> Expect.equal (Ok "foo") <| decodeString userId "{\"id\":\"foo\"}"


playlistIdTest : Test
playlistIdTest =
    test "playlistId" <|
        \_ -> Expect.equal (Ok "foo") <| decodeString playlistId "{\"id\":\"foo\"}"


imageTest : Test
imageTest =
    let
        params : List ( String, ( String, Image ) )
        params =
            [ ( "with size", ( imageJson, imageObject ) )
            , ( "without size", ( imageWithoutSizeJson, imageWithoutSizeObject ) )
            ]
    in
    parameterizedWithTitles "image" params <|
        \( json, decoded ) -> Expect.equal (Ok decoded) <| decodeString image json


spotifyUrlTest : Test
spotifyUrlTest =
    test "spotifyUrl" <|
        \_ ->
            Expect.equal (Ok "<some-url>") <|
                decodeString spotifyUrl "{\"external_urls\": {\"spotify\": \"<some-url>\"}}"


playlistTest : Test
playlistTest =
    test "playlist" <|
        \_ ->
            Expect.equal (Ok playlistObject) <|
                decodeString playlist playlistJson


pagingPlaylistTest : Test
pagingPlaylistTest =
    test "paging playlist" <|
        \_ ->
            Expect.equal (Ok <| pagingObject playlistObject) <|
                decodeString (paging playlist) <|
                    pagingString playlistJson


trackTest : Test
trackTest =
    let
        params : List ( String, ( String, Track ) )
        params =
            [ ( "invalid", ( noTrackJson, noTrackObject ) )
            , ( "valid", ( trackJson, trackObject ) )
            ]
    in
    parameterizedWithTitles "track" params <|
        \( json, obj ) ->
            Expect.equal (Ok obj) <|
                decodeString track json


pagingTrackTest : Test
pagingTrackTest =
    test "paging track" <|
        \_ ->
            Expect.equal (Ok <| pagingObject trackObject) <|
                decodeString (paging track) <|
                    pagingString trackJson
