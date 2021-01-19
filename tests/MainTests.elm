module MainTests exposing (..)

import Browser exposing (Document)
import Expect
import Html exposing (Html, div)
import Main exposing (Model, Msg, Page(..), subscriptions, view)
import Pages.Backup exposing (BackupModel)
import Set
import Test exposing (Test, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)
import TestHelpers exposing (parameterizedWithTitles)


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


viewTitleTest : Test
viewTitleTest =
    test "[view] Title is \"Spotify Backup\"" <|
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
