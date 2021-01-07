module RouteTests exposing (..)

import Expect
import Route exposing (Route(..), fromUrl)
import Test exposing (Test)
import TestHelpers exposing (parameterizedWithTitles)
import Url exposing (Protocol(..), Url)


basicUrl : Url
basicUrl =
    { protocol = Https
    , host = "spotify-backup.drako.guru"
    , port_ = Just 443
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


fromUrlTest : Test
fromUrlTest =
    let
        homeUrl =
            { basicUrl | path = "/" }

        notFoundUrl =
            { basicUrl | path = "/some/non/existing/path" }

        callbackWithTokenUrl =
            { basicUrl | path = "/callback", fragment = Just "token" }

        callbackWithErrorUrl =
            { basicUrl | path = "/callback", query = Just "error=shit" }

        backupUrl =
            { basicUrl | path = "/backup" }

        params : List ( String, ( Url, Route ) )
        params =
            [ ( "Home", ( homeUrl, Home ) )
            , ( "Not found", ( notFoundUrl, NotFound notFoundUrl ) )
            , ( "Callback with token", ( callbackWithTokenUrl, Callback Nothing (Just "token") ) )
            , ( "Callback with error", ( callbackWithErrorUrl, Callback (Just "shit") Nothing ) )
            , ( "Backup", ( backupUrl, Backup ) )
            ]
    in
    parameterizedWithTitles "route" params <|
        \( url, route ) -> Expect.equal route <| fromUrl url
