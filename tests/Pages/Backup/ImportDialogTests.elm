module Pages.Backup.ImportDialogTests exposing (..)

import Backup.Payloads exposing (Playlist)
import Backup.TestData exposing (playlistObject)
import Dict
import Expect
import Fuzz exposing (string)
import Pages.Backup.ImportDialog exposing (ImportModel, ImportMsg(..), update)
import Set exposing (Set)
import Test exposing (Test, fuzz, test)
import TestHelpers exposing (parameterized)
import Tuple exposing (first)


playlistObject2 : Playlist
playlistObject2 =
    { playlistObject | originalId = "<some-id-2>", name = "example-2" }


dummyImportModel : ImportModel
dummyImportModel =
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
    parameterized "update should not handle message" messages <|
        \message -> Expect.equal (Just dummyImportModel) <| first <| update message (Just dummyImportModel)


selectImportTest : Test
selectImportTest =
    let
        params : List ( String, Bool, Set String )
        params =
            [ ( playlistObject.originalId, True, Set.fromList [ playlistObject.originalId, playlistObject2.originalId ] )
            , ( playlistObject2.originalId, False, Set.empty )
            ]
    in
    parameterized "SelectForImport should select/unselect single playlist" params <|
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
    parameterized "SelectAllForImport should select/unselect all playlists" params <|
        \( select, expected ) ->
            Expect.equalSets expected <|
                Maybe.withDefault Set.empty <|
                    Maybe.map .selectedPlaylists <|
                        first <|
                            update (SelectAllForImport select) (Just dummyImportModel)


selectNonCollidingTest : Test
selectNonCollidingTest =
    test "SelectNonCollidingForImport should select playlists that have no collisions" <|
        \_ ->
            Expect.equalSets (Set.singleton playlistObject.originalId) <|
                Maybe.withDefault Set.empty <|
                    Maybe.map .selectedPlaylists <|
                        first <|
                            update SelectNonCollidingForImport (Just dummyImportModel)


renameImportTest : Test
renameImportTest =
    fuzz string "RenameImport should rename playlist for import" <|
        \name ->
            Expect.equal (Just name) <|
                Maybe.andThen (\{ renames } -> Dict.get playlistObject2.originalId renames) <|
                    first <|
                        update (RenameImport playlistObject2.originalId name) (Just dummyImportModel)
