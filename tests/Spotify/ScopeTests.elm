module Spotify.ScopeTests exposing (..)

import Expect
import Spotify.Scope exposing (Scope(..), toString)
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
