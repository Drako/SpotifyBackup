module Pages.HomeTests exposing (..)

import Browser exposing (UrlRequest(..))
import Expect
import Fuzz exposing (string)
import Pages.Home exposing (HomeMsg(..), redirectToExternal)
import Test exposing (Test, fuzz)


redirectToExternalTest : Test
redirectToExternalTest =
    fuzz string "redirectToExternal" <|
        \url -> Expect.equal (Authenticate <| External url) <| redirectToExternal url
