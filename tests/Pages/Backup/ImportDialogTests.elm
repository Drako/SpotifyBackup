module Pages.Backup.ImportDialogTests exposing (..)

import Backup.Payloads exposing (Playlist)
import Backup.TestData exposing (playlistObject)
import Dict
import Element exposing (layout)
import Expect
import Fuzz exposing (string)
import Html exposing (Html)
import Pages.Backup.ImportDialog exposing (ImportModel, ImportMsg(..), update, view)
import Set exposing (Set)
import Test exposing (Test, fuzz, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector, class, containing, text)
import TestHelpers exposing (parameterized, parameterizedWithTitles)
import Tuple exposing (first)


playlistObject2 : Playlist
playlistObject2 =
    { playlistObject | originalId = "<some-id-2>", name = "example-2" }


dummyImportModel : ImportModel
dummyImportModel =
    -- the dummy model contains 2 playlists, with the 2nd being already selected, but also already existing
    { playlists = [ playlistObject, playlistObject2 ]
    , renames =
        Dict.fromList
            [ ( playlistObject.originalId, playlistObject.name )
            , ( playlistObject2.originalId, playlistObject2.name )
            ]
    , selectedPlaylists = Set.singleton playlistObject2.originalId
    , existing = Set.singleton playlistObject2.name
    }


noModelTest : Test
noModelTest =
    let
        messages : List ImportMsg
        messages =
            [ CloseImport
            , RenameImport "foo" "bar"
            , SelectAllForImport True
            , SelectNonCollidingForImport
            , SelectForImport "foo" True
            , ImportSelected
            ]
    in
    parameterized "update should do nothing without model" messages <|
        \message -> Expect.equal Nothing <| first <| update message Nothing


unhandledTest : Test
unhandledTest =
    let
        messages : List ImportMsg
        messages =
            [ CloseImport, ImportSelected ]
    in
    parameterized "[update] should not handle message" messages <|
        \message -> Expect.equal (Just dummyImportModel) <| first <| update message (Just dummyImportModel)


selectImportTest : Test
selectImportTest =
    let
        params : List ( String, Bool, Set String )
        params =
            [ ( playlistObject.originalId, True, Set.fromList [ playlistObject.originalId, playlistObject2.originalId ] )
            , ( playlistObject.originalId, False, Set.fromList [ playlistObject2.originalId ] )
            , ( playlistObject2.originalId, True, Set.fromList [ playlistObject2.originalId ] )
            , ( playlistObject2.originalId, False, Set.empty )
            ]
    in
    parameterized "[update] SelectForImport should select/unselect single playlist" params <|
        \( id, select, expected ) ->
            Expect.equalSets expected <|
                Maybe.withDefault Set.empty <|
                    Maybe.map .selectedPlaylists <|
                        first <|
                            update (SelectForImport id select) (Just dummyImportModel)


selectAllImportTest : Test
selectAllImportTest =
    let
        params : List ( Bool, Set String )
        params =
            [ ( True, Set.fromList [ playlistObject.originalId, playlistObject2.originalId ] )
            , ( False, Set.empty )
            ]
    in
    parameterized "[update] SelectAllForImport should select/unselect all playlists" params <|
        \( select, expected ) ->
            Expect.equalSets expected <|
                Maybe.withDefault Set.empty <|
                    Maybe.map .selectedPlaylists <|
                        first <|
                            update (SelectAllForImport select) (Just dummyImportModel)


selectNonCollidingTest : Test
selectNonCollidingTest =
    test "[update] SelectNonCollidingForImport should select playlists that have no collisions" <|
        \_ ->
            Expect.equalSets (Set.singleton playlistObject.originalId) <|
                Maybe.withDefault Set.empty <|
                    Maybe.map .selectedPlaylists <|
                        first <|
                            update SelectNonCollidingForImport (Just dummyImportModel)


renameImportTest : Test
renameImportTest =
    fuzz string "[update] RenameImport should rename playlist for import" <|
        \name ->
            Expect.equal (Just name) <|
                Maybe.andThen (\{ renames } -> Dict.get playlistObject2.originalId renames) <|
                    first <|
                        update (RenameImport playlistObject2.originalId name) (Just dummyImportModel)


dialogHiddenWithoutModelTest : Test
dialogHiddenWithoutModelTest =
    test "[view] The Import dialog is hidden if there is no model" <|
        \_ -> Expect.equal Element.none <| view Nothing


importButton : List Selector
importButton =
    [ class "sbt", containing [ text "Import." ] ]


importIsDisabledIfThereAreCollisionsTest : Test
importIsDisabledIfThereAreCollisionsTest =
    let
        rendered : Html ImportMsg
        rendered =
            layout [] <| view <| Just dummyImportModel
    in
    test "[view] The import button is disabled if there are collisions" <|
        \_ ->
            Query.fromHtml rendered
                |> Query.find importButton
                |> Event.simulate Event.click
                |> Event.toResult
                |> Expect.notEqual (Ok ImportSelected)


importIsDisabledIfThereAreNoSelectionsTest : Test
importIsDisabledIfThereAreNoSelectionsTest =
    let
        model : ImportModel
        model =
            { dummyImportModel
                | selectedPlaylists = Set.empty
                , existing = Set.empty
            }

        rendered : Html ImportMsg
        rendered =
            layout [] <| view <| Just model
    in
    test "[view] The import button is disabled if there are no selections" <|
        \_ ->
            Query.fromHtml rendered
                |> Query.find importButton
                |> Event.simulate Event.click
                |> Event.toResult
                |> Expect.notEqual (Ok ImportSelected)


importIsEnabledIfThereAreNoCollisionsAndSomethingIsSelectedTest : Test
importIsEnabledIfThereAreNoCollisionsAndSomethingIsSelectedTest =
    let
        baseModel : ImportModel
        baseModel =
            { dummyImportModel
                | existing = Set.empty
            }

        params : List ( String, Set String )
        params =
            [ ( "only first playlist", Set.singleton playlistObject.originalId )
            , ( "only second playlist", Set.singleton playlistObject2.originalId )
            , ( "both playlists", Set.fromList [ playlistObject.originalId, playlistObject2.originalId ] )
            ]
    in
    parameterizedWithTitles "[view] The import button is enabled if there are no collisions and something is selected" params <|
        \selected ->
            let
                model : ImportModel
                model =
                    { baseModel | selectedPlaylists = selected }

                rendered : Html ImportMsg
                rendered =
                    layout [] <| view <| Just model
            in
            Query.fromHtml rendered
                |> Query.find importButton
                |> Event.simulate Event.click
                |> Event.toResult
                |> Expect.equal (Ok ImportSelected)
