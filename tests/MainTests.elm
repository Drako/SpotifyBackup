module MainTests exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Expect
import Html exposing (Html, div)
import Http exposing (Error(..))
import Main exposing (Model, Msg(..), Page(..), init, main, subscriptions, update, view)
import Pages.Backup exposing (BackupModel, BackupMsg(..))
import Pages.Home exposing (HomeMsg(..))
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


urlPath : String -> Url
urlPath path =
    { basicUrl | path = path }


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

        params : List ( String, Url )
        params =
            [ ( "path: /", urlPath "/" )
            , ( "path: /bs", urlPath "/bs" )

            -- callback with valid token triggers loading of user id before redirecting to backup page
            -- sadly this is not reflected in the Model, so we still just get the homePage model here
            , ( "path: /callback#access_token=foo&token_type=token&expires_in=10 (HomePage)"
              , { basicUrl
                    | path = "/callback"
                    , fragment = Just "access_token=foo&token_type=token&expires_in=10"
                }
              )

            -- callback with invalid token redirects to home page
            , ( "path: /callback#token"
              , { basicUrl | path = "/callback", fragment = Just "token" }
              )

            -- callback with error redirects to home page
            , ( "path: /callback?error=foo"
              , { basicUrl | path = "/callback", query = Just "error=foo" }
              )

            -- backup cannot be accessed directly, when calling the URL one is redirected to the home page
            , ( "path: /backup", urlPath "/backup" )
            ]
    in
    parameterizedWithTitles "[init] model should be initialized according to URL" params <|
        \url -> Expect.equal homePage <| first <| init url Nothing


updateTest : Test
updateTest =
    let
        params : List ( String, ( Model, Msg, Model ) )
        params =
            [ ( "received user (ok)"
              , ( dummyModel
                , ReceivedUser dummyBackupModel.token (Ok "<user-id>")
                , { dummyModel | page = BackupPage dummyBackupModel }
                )
              )
            , ( "received user (err)", ( dummyModel, ReceivedUser dummyBackupModel.token (Err Timeout), dummyModel ) )
            , ( "navigated", ( dummyModel, Navigated (urlPath "/"), dummyModel ) )
            , ( "navigated to callback"
              , ( { dummyModel | page = BackupPage dummyBackupModel }
                , Navigated (urlPath "/backup")
                , { dummyModel | page = BackupPage { dummyBackupModel | playlists = [], status = Just "Retrieving playlists." } }
                )
              )
            , ( "redirect", ( dummyModel, Redirect (Internal basicUrl), dummyModel ) )
            , ( "home -> authenticate", ( dummyModel, HomeMessage (Authenticate (External "")), dummyModel ) )
            , ( "backup"
              , ( { dummyModel | page = BackupPage dummyBackupModel }
                , BackupMessage Enter
                , { dummyModel | page = BackupPage { dummyBackupModel | playlists = [], status = Just "Retrieving playlists." } }
                )
              )
            , ( "backup message on home page", ( dummyModel, BackupMessage Enter, dummyModel ) )
            ]
    in
    parameterizedWithTitles "[update] model should be updated according to message" params <|
        \( inputModel, message, expected ) -> Expect.equal expected <| first <| update message inputModel
