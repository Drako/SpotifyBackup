module Spotify.Payloads exposing (..)


type alias Image =
    { url : String
    , width : Maybe Int
    , height : Maybe Int
    }


type alias Playlist =
    { href : String
    , url : String
    , id : String
    , images : List Image
    , name : String
    , isPublic : Bool
    , tracks : { href : String, total : Int }
    , owner : { displayName : Maybe String, id : String, url : String }
    }


type alias Track =
    { name : String
    , album : String
    , artists : List String
    , url : Maybe String
    , uri : String
    }


visibilityToString : Bool -> String
visibilityToString isPublic =
    if isPublic then
        "public"

    else
        "private"


type alias Paging a =
    { href : String
    , items : List a
    , limit : Int
    , next : Maybe String
    , offset : Int
    , previous : Maybe String
    , total : Int
    }
