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


type alias Paging a =
    { href : String
    , items : List a
    , limit : Int
    , next : Maybe String
    , offset : Int
    , previous : Maybe String
    , total : Int
    }
