module Spotify.Decoder exposing (..)

import Json.Decode exposing (Decoder, bool, field, int, list, map, map2, map3, map6, map7, maybe, string)
import Spotify.Payloads exposing (Image, Paging, Playlist)


image : Decoder Image
image =
    map3 Image
        (field "url" string)
        (field "width" <| maybe int)
        (field "height" <| maybe int)


playlist : Decoder Playlist
playlist =
    map6 Playlist
        (field "href" string)
        (field "id" string)
        (field "images" <| list image)
        (field "name" string)
        (field "public" <| (maybe bool |> map (Maybe.withDefault False)))
        (field "tracks" <|
            map2 (\href total -> { href = href, total = total })
                (field "href" string)
                (field "total" int)
        )


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
