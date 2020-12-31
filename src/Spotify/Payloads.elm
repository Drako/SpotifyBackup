module Spotify.Payloads exposing (..)


type alias Image =
    { url : String
    , width : Maybe Int
    , height : Maybe Int
    }


type alias Playlist =
    { href : String
    , id : String
    , images : List Image
    , name : String
    , isPublic : Bool
    , tracks : { href : String, total : Int }
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
