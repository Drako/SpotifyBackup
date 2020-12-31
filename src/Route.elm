module Route exposing (Route(..), fromUrl)

import Maybe exposing (withDefault)
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, fragment, map, oneOf, parse, s, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Callback (Maybe String) (Maybe String)
    | NotFound Url


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map Callback (s "callback" <?> Query.string "error" </> fragment identity)
        ]


fromUrl : Url -> Route
fromUrl url =
    parse route url
        |> withDefault (NotFound url)
