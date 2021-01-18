module Spotify.Token exposing (Token, fromFragment)

import Dict exposing (get)
import List exposing (filterMap, map)
import Maybe exposing (withDefault)
import String exposing (split)
import Url exposing (Url)


type alias Token =
    { accessToken : String
    , tokenType : String
    , expiresIn : Int
    }


urlDecode : String -> String
urlDecode string =
    Url.percentDecode string |> withDefault string


partsToKeyValue : List (List String) -> List ( String, String )
partsToKeyValue =
    filterMap
        (\kv ->
            case kv of
                [ k, v ] ->
                    Just ( urlDecode k, urlDecode v )

                _ ->
                    Nothing
        )


fromFragment : String -> Maybe Token
fromFragment fragment =
    let
        kvs =
            fragment
                |> split "&"
                |> map (split "=")
                |> partsToKeyValue
                |> Dict.fromList
    in
    Maybe.map3 (\tok tt exp -> { accessToken = tok, tokenType = tt, expiresIn = exp })
        (get "access_token" kvs)
        (get "token_type" kvs)
        (get "expires_in" kvs |> Maybe.andThen String.toInt)
