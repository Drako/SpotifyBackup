module Spotify.PayloadsTests exposing (..)

import Expect
import Spotify.Payloads exposing (visibilityToString)
import Test exposing (Test)
import TestHelpers exposing (parameterizedWithTitles)


visibilityToStringTest : Test
visibilityToStringTest =
    let
        params : List ( String, ( Bool, String ) )
        params =
            [ ( "public", ( True, "public" ) )
            , ( "private", ( False, "private" ) )
            ]
    in
    parameterizedWithTitles "visibilityToString" params <|
        \( isPublic, visibility ) -> Expect.equal visibility <| visibilityToString isPublic
