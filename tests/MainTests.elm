module MainTests exposing (..)

import Browser exposing (Document)
import Expect
import Html exposing (Html, div)
import Main exposing (Model, Msg, Page(..), init, main, subscriptions, view)
import Pages.Backup exposing (BackupModel)
import RouteTests exposing (basicUrl)
import Set
import Test exposing (Test, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)
import TestHelpers exposing (parameterizedWithTitles)
import Tuple exposing (first, second)
import Url exposing (Url)


dummyModel : Model
dummyModel =
    { key = Nothing, page = HomePage }


dummyBackupModel : BackupModel
dummyBackupModel =
    { token =
        { accessToken = "<token>"
        , tokenType = "token"
        , expiresIn = 10
        }
    , userId = "<user-id>"
    , error = Nothing
    , status = Nothing
    , playlists = []
    , selectedPlaylists = Set.empty
    , importDialog = Nothing
    }


callView : (Document Msg -> a) -> Model -> a
callView toResult model =
    toResult <|
        view
            (\_ -> div [] [ Html.text "Home" ])
            (\_ -> div [] [ Html.text "Backup" ])
            model


noSubscriptionsTest : Test
noSubscriptionsTest =
    test "[subscriptions] there are no subscriptions" <|
        \_ -> Expect.equal Sub.none <| subscriptions dummyModel


mainTest : Test
mainTest =
    test "[main] application can be created" <|
        \_ -> Expect.equal 1 <| second <| ( main, 1 )


viewTitleTest : Test
viewTitleTest =
    test "[view] the title is \"Spotify Backup\"" <|
        \_ -> Expect.equal "Spotify Backup" <| callView .title dummyModel


viewTest : Test
viewTest =
    let
        params : List ( String, ( Model, String ) )
        params =
            [ ( "HomePage", ( dummyModel, "Home" ) )
            , ( "BackupPage", ( { dummyModel | page = BackupPage dummyBackupModel }, "Backup" ) )
            ]
    in
    parameterizedWithTitles "[view] the expected page is being viewed" params <|
        \( model, expected ) ->
            (Query.fromHtml <| callView (\doc -> div [] doc.body) model)
                |> Query.find [ tag "div" ]
                |> Query.has [ text expected ]


initTest : Test
initTest =
    let
        homePage : Model
        homePage =
            { key = Nothing, page = HomePage }

        urlPath : String -> Url
        urlPath path =
            { basicUrl | path = path }

        params : List ( String, ( Url, Model ) )
        params =
            [ ( "path: / (HomePage)", ( urlPath "/", homePage ) )
            , ( "path: /bs (NotFound -> HomePage)", ( urlPath "/bs", homePage ) )

            -- callback with valid token triggers loading of user id before redirecting to backup page
            , ( "path: /callback#access_token=foo&token_type=token&expires_in=10 (HomePage)"
              , ( { basicUrl
                    | path = "/callback"
                    , fragment = Just "access_token=foo&token_type=token&expires_in=10"
                  }
                , homePage
                )
              )

            -- callback with invalid token redirects to home page
            , ( "path: /callback#token (HomePage)"
              , ( { basicUrl | path = "/callback", fragment = Just "token" }, homePage )
              )

            -- callback with error redirects to home page
            , ( "path: /callback?error=foo (HomePage)"
              , ( { basicUrl | path = "/callback", query = Just "error=foo" }, homePage )
              )

            -- backup cannot be accessed directly, when calling the URL one is redirected to the home page
            , ( "path: /backup (HomePage)", ( urlPath "/backup", homePage ) )
            ]
    in
    parameterizedWithTitles "[init] model should be initialized according to URL" params <|
        \( url, expected ) -> Expect.equal expected <| first <| init url Nothing
