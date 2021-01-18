module Spotify.TokenTests exposing (..)

import Expect
import Spotify.Token exposing (Token, fromFragment)
import Test exposing (Test)
import TestHelpers exposing (parameterizedWithTitles)


fromFragmentInvalidTokenTest : Test
fromFragmentInvalidTokenTest =
    let
        params : List ( String, ( String, Maybe Token ) )
        params =
            [ ( "invalid", ( "access_token&token_type=token", Nothing ) )
            , ( "valid"
              , ( "access_token=foo&token_type=token&expires_in=10"
                , Just { accessToken = "foo", tokenType = "token", expiresIn = 10 }
                )
              )
            , ( "valid with empty value"
              , ( "access_token=&token_type=empty&expires_in=23"
                , Just { accessToken = "", tokenType = "empty", expiresIn = 23 }
                )
              )
            ]
    in
    parameterizedWithTitles "fromFragment" params <|
        \( url, route ) -> Expect.equal route <| fromFragment url
