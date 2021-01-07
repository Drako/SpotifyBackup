module Spotify.TestData exposing (..)

import Spotify.Payloads exposing (Image, Paging, Playlist, Track)


imageJson : String
imageJson =
    "{\"url\":\"<image-url>\", \"width\": 64, \"height\": 64}"


imageObject : Image
imageObject =
    { url = "<image-url>", width = Just 64, height = Just 64 }


imageWithoutSizeJson : String
imageWithoutSizeJson =
    "{\"url\":\"<image-url>\"}"


imageWithoutSizeObject : Image
imageWithoutSizeObject =
    { imageObject | width = Nothing, height = Nothing }


playlistObject : Playlist
playlistObject =
    { href = "<playlist-webapi-link>"
    , url = "<some-url>"
    , id = "<some-id>"
    , images = [ imageObject ]
    , name = "example"
    , isPublic = False
    , tracks = { href = "<tracks-webapi-link>", total = 1 }
    , owner = { displayName = Nothing, id = "<some-id>", url = "<some-url>" }
    }


playlistJson : String
playlistJson =
    "{\"href\": \"<playlist-webapi-link>\","
        ++ "\"external_urls\": {\"spotify\": \"<some-url>\"},"
        ++ "\"id\": \"<some-id>\", \"images\": ["
        ++ imageJson
        ++ "], \"name\": \"example\", \"public\": false, \"tracks\": {"
        ++ "\"href\": \"<tracks-webapi-link>\", \"total\": 1"
        ++ "}, \"owner\": {"
        ++ "\"display_name\": null, \"id\": \"<some-id>\", \"external_urls\": {\"spotify\": \"<some-url>\"}"
        ++ "}}"


trackJson : String
trackJson =
    "{\"track\":{"
        ++ "\"name\": \"Umbrella\", \"album\": {\"name\": \"Umbrella\"},"
        ++ "\"artists\": [{\"name\": \"Ember Island\"}],"
        ++ "\"external_urls\": {\"spotify\": \"https://open.spotify.com/track/2FX5HPP1FNNuLlt2UU9os7\"},"
        ++ "\"uri\": \"spotify:track:2FX5HPP1FNNuLlt2UU9os7\""
        ++ "}}"


trackObject : Track
trackObject =
    { name = "Umbrella"
    , album = "Umbrella"
    , artists = [ "Ember Island" ]
    , url = Just "https://open.spotify.com/track/2FX5HPP1FNNuLlt2UU9os7"
    , uri = "spotify:track:2FX5HPP1FNNuLlt2UU9os7"
    }


noTrackJson : String
noTrackJson =
    -- some json object not containing the "track" key
    "{}"


noTrackObject : Track
noTrackObject =
    { name = "", album = "", artists = [], url = Nothing, uri = "" }


pagingString : String -> String
pagingString str =
    "{\"href\": \"<paging-webapi-link>\", \"items\": ["
        ++ str
        ++ "], \"limit\": 1, \"next\": null, \"offset\": 0, \"previous\": null, \"total\": 1}"


pagingObject : a -> Paging a
pagingObject obj =
    { href = "<paging-webapi-link>"
    , items = [ obj ]
    , limit = 1
    , next = Nothing
    , offset = 0
    , previous = Nothing
    , total = 1
    }
