module Spotify.Decoder exposing (..)

import Json.Decode exposing (Decoder, at, bool, field, int, list, map, map2, map3, map5, map7, map8, maybe, string)
import Spotify.Payloads exposing (Image, Paging, Playlist, Track)


userId : Decoder String
userId =
    field "id" string


playlistId : Decoder String
playlistId =
    field "id" string


image : Decoder Image
image =
    map3 Image
        (field "url" string)
        (maybe <| field "width" int)
        (maybe <| field "height" int)


spotifyUrl : Decoder String
spotifyUrl =
    at [ "external_urls", "spotify" ] string


playlist : Decoder Playlist
playlist =
    map8 Playlist
        (field "href" string)
        spotifyUrl
        (field "id" string)
        (field "images" <| list image)
        (field "name" string)
        (field "public" <| (maybe bool |> map (Maybe.withDefault False)))
        (field "tracks" <|
            map2 (\href total -> { href = href, total = total })
                (field "href" string)
                (field "total" int)
        )
        (field "owner" <|
            map3 (\name id url -> { displayName = name, id = id, url = url })
                (field "display_name" <| maybe string)
                (field "id" string)
                spotifyUrl
        )


track : Decoder Track
track =
    map (\t -> Maybe.withDefault { name = "", album = "", artists = [], url = Nothing, uri = "" } t) <|
        maybe <|
            field "track" <|
                map5 Track
                    (field "name" string)
                    (at [ "album", "name" ] string)
                    (field "artists" <| list <| field "name" string)
                    (maybe spotifyUrl)
                    (field "uri" string)


paging : Decoder a -> Decoder (Paging a)
paging itemDecoder =
    map7 Paging
        (field "href" string)
        (field "items" <| list itemDecoder)
        (field "limit" int)
        (field "next" <| maybe string)
        (field "offset" int)
        (field "previous" <| maybe string)
        (field "total" int)
