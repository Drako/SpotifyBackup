module Spotify.ScopeTests exposing (..)

import Expect
import Spotify.Scope exposing (Scope(..), all, toQueryParams, toString)
import Test exposing (Test)
import TestHelpers exposing (parameterized)


toStringTest : Test
toStringTest =
    let
        params : List ( Scope, String )
        params =
            [ ( PlaylistReadPrivate, "playlist-read-private" )
            , ( PlaylistModifyPrivate, "playlist-modify-private" )
            , ( PlaylistModifyPublic, "playlist-modify-public" )
            , ( PlaylistReadCollaborative, "playlist-read-collaborative" )
            ]
    in
    parameterized "toString" params <|
        \( scope, scopeString ) -> Expect.equal scopeString <| toString scope


toQueryParamsTest : Test
toQueryParamsTest =
    let
        params : List ( List Scope, String )
        params =
            [ ( [], "" )
            , ( [ PlaylistReadPrivate, PlaylistModifyPrivate ], "playlist-read-private%20playlist-modify-private" )
            , ( all, "playlist-read-private%20playlist-modify-private%20playlist-modify-public%20playlist-read-collaborative" )
            ]
    in
    parameterized "toQueryParams" params <|
        \( scopes, scopesParam ) -> Expect.equal scopesParam <| toQueryParams scopes
