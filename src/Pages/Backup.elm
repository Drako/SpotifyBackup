module Pages.Backup exposing (BackupModel, BackupMsg(..), init, update, view)

import Backup.Decoder exposing (playlists)
import Backup.Encoder exposing (playlistToJson, playlistsToJson)
import Backup.Payloads as Backup exposing (spotifyToBackup)
import Char exposing (isAlphaNum)
import Dialog
import Dict exposing (Dict)
import Element
    exposing
        ( Column
        , Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , image
        , inFront
        , layout
        , maximum
        , newTabLink
        , none
        , padding
        , paddingEach
        , px
        , row
        , scrollbarY
        , shrink
        , table
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (checkbox, defaultCheckbox, labelHidden, labelLeft)
import Element.Region as Region
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html)
import Http exposing (Error)
import Json.Decode as Decode exposing (decodeString)
import List exposing (reverse)
import Set exposing (Set)
import Spotify.Api as Api exposing (errorToString)
import Spotify.Payloads exposing (Paging, Playlist, Track, visibilityToString)
import Spotify.Token exposing (Token)
import Style
    exposing
        ( black
        , disabledButton
        , edges
        , heading
        , headingRow
        , lightRed
        , spotifyBackground
        , spotifyButton
        , spotifyForeground
        , white
        )
import Task


type BackupMsg
    = Enter
    | GotPlaylists (Result Error (Paging Playlist))
    | PlaylistSelected String Bool
    | SelectAll Bool
    | Export Playlist
    | ExportSelected
    | ExportAll
    | GotTracks Playlist (List Track) (Result Error (Paging Track))
    | GotTracksMultiPlaylist (List Backup.Playlist) Playlist (List Playlist) (List Track) (Result Error (Paging Track))
    | Import
    | ImportFileSelected File
    | ImportFileLoaded String
    | CloseImport
    | RenameImport String String
    | SelectAllForImport Bool
    | SelectForImport String Bool
    | ImportSelected
    | PlaylistCreated (List Backup.Playlist) (List Backup.Playlist) (List Track) (Result Error String)
    | TracksAdded (List Backup.Playlist) String (List Backup.Playlist) (List Track) (Result Error ())


type alias ImportModel =
    { playlists : List Backup.Playlist
    , renames : Dict String String
    , selectedPlaylists : Set String
    , existing : Set String
    }


type alias BackupModel =
    { token : Token
    , userId : String
    , error : Maybe String
    , status : Maybe String
    , playlists : List Playlist
    , selectedPlaylists : Set String
    , importDialog : Maybe ImportModel
    }


init : Token -> String -> BackupModel
init tok userId =
    { playlists = []
    , selectedPlaylists = Set.empty
    , error = Nothing
    , status = Nothing
    , token = tok
    , userId = userId
    , importDialog = Nothing
    }


isAllowedChar : Char -> Bool
isAllowedChar c =
    isAlphaNum c || c == ' ' || c == '-' || c == '_'


filenameForPlaylist : Playlist -> String
filenameForPlaylist pl =
    (pl.name
        |> String.trim
        |> String.filter isAllowedChar
    )
        ++ ".json"


exportProgress : List Backup.Playlist -> List Playlist -> String
exportProgress done todo =
    let
        doneCount =
            List.length done

        todoCount =
            List.length todo

        current =
            doneCount + 1

        fullCount =
            current + todoCount
    in
    "Retrieving playlist " ++ String.fromInt current ++ "/" ++ String.fromInt fullCount ++ "..."


importProgress : List Backup.Playlist -> List Backup.Playlist -> String
importProgress done todo =
    let
        doneCount =
            List.length done

        todoCount =
            List.length todo

        current =
            doneCount + 1

        fullCount =
            current + todoCount
    in
    "Importing playlist " ++ String.fromInt current ++ "/" ++ String.fromInt fullCount ++ "..."


retrievalFailure : BackupModel -> String -> Error -> ( BackupModel, Cmd BackupMsg )
retrievalFailure model what err =
    ( { model
        | status = Nothing
        , error = Just <| "Failed retrieving " ++ what ++ ": " ++ errorToString err
      }
    , Cmd.none
    )


noCollisions : ImportModel -> Bool
noCollisions { renames, existing, selectedPlaylists, playlists } =
    playlists
        |> List.filter (\{ originalId } -> Set.member originalId selectedPlaylists)
        |> List.filterMap (\{ name } -> Dict.get name renames)
        |> List.map String.trim
        |> List.any (\name -> Set.member name existing)
        |> not


importDialog : BackupModel -> Element BackupMsg
importDialog model =
    Dialog.view <|
        Maybe.map
            (\importModel ->
                { closeMessage = Just CloseImport
                , maskAttributes = []
                , containerAttributes =
                    [ padding 10
                    , Background.color spotifyBackground
                    , Border.color white
                    , Border.width 1
                    , centerX
                    , centerY
                    ]
                , headerAttributes = []
                , bodyAttributes = []
                , footerAttributes = []
                , header = Just (el [ Region.heading 1 ] <| text "Select playlists for import.")
                , body =
                    Just <|
                        el
                            [ height <| maximum 800 fill
                            , scrollbarY
                            ]
                        <|
                            table []
                                { data = importModel.playlists
                                , columns =
                                    [ importNameColumn
                                    , importRenameColumn importModel
                                    , importImportColumn importModel
                                    ]
                                }
                , footer =
                    Just <|
                        el [ centerX ]
                            (if (not <| Set.isEmpty importModel.selectedPlaylists) && noCollisions importModel then
                                spotifyButton "Import." <| Just ImportSelected

                             else
                                disabledButton "Import."
                            )
                }
            )
            model.importDialog


hasValue : Maybe a -> Bool
hasValue mb =
    case mb of
        Just _ ->
            True

        Nothing ->
            False


decodeBackup : BackupModel -> String -> ( BackupModel, Cmd BackupMsg )
decodeBackup model content =
    let
        result =
            decodeString playlists content

        newModel =
            case result of
                Ok playlists ->
                    { model
                        | error = Nothing
                        , status = Just "Showing import selection..."
                        , importDialog =
                            Just
                                { playlists = playlists
                                , renames = Dict.fromList <| List.map (\{ name } -> ( name, name )) playlists
                                , selectedPlaylists = Set.empty
                                , existing = Set.fromList <| List.map .name playlists
                                }
                    }

                Err err ->
                    { model | status = Nothing, error = Just <| Decode.errorToString err }
    in
    ( newModel, Cmd.none )


refresh : BackupModel -> ( BackupModel, Cmd BackupMsg )
refresh model =
    ( { model | playlists = [], status = Just "Retrieving playlists." }
    , Api.fetchPlaylists model.token GotPlaylists
    )


update : BackupMsg -> BackupModel -> ( BackupModel, Cmd BackupMsg )
update msg model =
    case msg of
        Enter ->
            refresh model

        CloseImport ->
            ( { model | status = Nothing, importDialog = Nothing }, Cmd.none )

        SelectForImport id selected ->
            case model.importDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just importModel ->
                    if selected then
                        ( { model
                            | importDialog = Just { importModel | selectedPlaylists = Set.insert id importModel.selectedPlaylists }
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | importDialog = Just { importModel | selectedPlaylists = Set.remove id importModel.selectedPlaylists }
                          }
                        , Cmd.none
                        )

        SelectAllForImport selected ->
            case model.importDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just importModel ->
                    if selected then
                        ( { model
                            | importDialog = Just { importModel | selectedPlaylists = Set.fromList <| List.map .id model.playlists }
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | importDialog = Just { importModel | selectedPlaylists = Set.empty }
                          }
                        , Cmd.none
                        )

        RenameImport name rename ->
            case model.importDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just importModel ->
                    ( { model
                        | importDialog = Just { importModel | renames = Dict.insert name rename importModel.renames }
                      }
                    , Cmd.none
                    )

        Import ->
            ( model, Select.file [ "application/json" ] ImportFileSelected )

        ImportFileSelected file ->
            ( { model | status = Just <| "Reading file: " ++ File.name file ++ "..." }
            , Task.perform ImportFileLoaded (File.toString file)
            )

        ImportFileLoaded content ->
            decodeBackup model content

        PlaylistSelected id selected ->
            if selected then
                ( { model | selectedPlaylists = Set.insert id model.selectedPlaylists }, Cmd.none )

            else
                ( { model | selectedPlaylists = Set.remove id model.selectedPlaylists }, Cmd.none )

        SelectAll selected ->
            if selected then
                ( { model | selectedPlaylists = Set.fromList <| List.map .id model.playlists }, Cmd.none )

            else
                ( { model | selectedPlaylists = Set.empty }, Cmd.none )

        GotPlaylists result ->
            case result of
                Ok playlists ->
                    case playlists.next of
                        Just _ ->
                            ( { model | playlists = model.playlists ++ playlists.items }
                            , Api.fetchMorePlaylists model.token playlists GotPlaylists
                            )

                        Nothing ->
                            ( { model | playlists = model.playlists ++ playlists.items, status = Nothing }, Cmd.none )

                Err error ->
                    retrievalFailure model "Playlists" error

        GotTracks playlist prevTracks result ->
            case result of
                Ok tracks ->
                    let
                        allTracks =
                            prevTracks ++ tracks.items
                    in
                    case tracks.next of
                        Nothing ->
                            ( model
                            , Download.string (filenameForPlaylist playlist) "application/json" <|
                                playlistToJson <|
                                    spotifyToBackup playlist allTracks
                            )

                        Just _ ->
                            ( model, Api.fetchMoreTracks model.token tracks <| GotTracks playlist allTracks )

                Err error ->
                    retrievalFailure model "Tracks" error

        GotTracksMultiPlaylist done current next prevTracks result ->
            case result of
                Ok tracks ->
                    let
                        allTracks =
                            prevTracks ++ tracks.items
                    in
                    case tracks.next of
                        Nothing ->
                            case next of
                                pl :: remaining ->
                                    let
                                        newDone =
                                            spotifyToBackup current allTracks :: done
                                    in
                                    ( { model | status = Just <| exportProgress newDone remaining }
                                    , Api.fetchTracks model.token pl <|
                                        GotTracksMultiPlaylist
                                            newDone
                                            pl
                                            remaining
                                            []
                                    )

                                _ ->
                                    ( { model | status = Nothing }
                                    , Download.string "spotify-playlists.json" "application/json" <|
                                        playlistsToJson <|
                                            reverse (spotifyToBackup current allTracks :: done)
                                    )

                        Just _ ->
                            ( model
                            , Api.fetchMoreTracks model.token tracks <|
                                GotTracksMultiPlaylist done current next allTracks
                            )

                Err error ->
                    retrievalFailure model "Tracks" error

        ExportAll ->
            case model.playlists of
                pl :: remainingPlaylists ->
                    ( { model | status = Just <| exportProgress [] remainingPlaylists }
                    , Api.fetchTracks model.token pl <| GotTracksMultiPlaylist [] pl remainingPlaylists []
                    )

                _ ->
                    -- no playlists at all
                    ( model, Cmd.none )

        ExportSelected ->
            let
                selectedPlaylists =
                    List.filter (\{ id } -> Set.member id model.selectedPlaylists) model.playlists
            in
            case selectedPlaylists of
                pl :: remainingPlaylists ->
                    ( { model | status = Just <| exportProgress [] remainingPlaylists }
                    , Api.fetchTracks model.token pl <| GotTracksMultiPlaylist [] pl remainingPlaylists []
                    )

                _ ->
                    -- no playlists selected
                    ( model, Cmd.none )

        ImportSelected ->
            case model.importDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just importModel ->
                    let
                        selectedPlaylists =
                            importModel.playlists
                                |> List.filter (\{ originalId } -> Set.member originalId importModel.selectedPlaylists)
                                |> List.map
                                    (\pl ->
                                        { pl
                                            | name = Dict.get pl.name importModel.renames |> Maybe.withDefault pl.name |> String.trim
                                            , tracks = List.filter (\{ url } -> hasValue url) pl.tracks
                                        }
                                    )
                    in
                    case selectedPlaylists of
                        pl :: remaining ->
                            ( { model
                                | importDialog = Nothing
                                , status = Just <| importProgress [] remaining
                              }
                            , Api.createPlaylist model.token model.userId pl.name <|
                                PlaylistCreated [] remaining pl.tracks
                            )

                        _ ->
                            ( { model | importDialog = Nothing, status = Nothing }, Cmd.none )

        TracksAdded done playlistId todo tracks result ->
            case result of
                Err error ->
                    ( { model | status = Nothing, error = Just <| errorToString error }, Cmd.none )

                Ok _ ->
                    let
                        currentTracks =
                            List.take 100 tracks

                        remainingTracks =
                            List.drop 100 tracks
                    in
                    if List.isEmpty currentTracks then
                        case todo of
                            pl :: remaining ->
                                let
                                    finished =
                                        pl :: done
                                in
                                ( { model
                                    | importDialog = Nothing
                                    , status = Just <| importProgress finished remaining
                                  }
                                , Api.createPlaylist model.token model.userId pl.name <|
                                    PlaylistCreated finished remaining pl.tracks
                                )

                            _ ->
                                -- done
                                refresh model

                    else
                        ( model
                        , Api.appendToPlaylist model.token playlistId currentTracks <|
                            TracksAdded done playlistId todo remainingTracks
                        )

        PlaylistCreated done todo tracks result ->
            case result of
                Err error ->
                    ( { model | status = Nothing, error = Just <| errorToString error }, Cmd.none )

                Ok playlistId ->
                    let
                        currentTracks =
                            List.take 100 tracks

                        remainingTracks =
                            List.drop 100 tracks
                    in
                    if List.isEmpty currentTracks then
                        case todo of
                            pl :: remaining ->
                                let
                                    finished =
                                        pl :: done
                                in
                                ( { model
                                    | importDialog = Nothing
                                    , status = Just <| importProgress finished remaining
                                  }
                                , Api.createPlaylist model.token model.userId pl.name <|
                                    PlaylistCreated finished remaining pl.tracks
                                )

                            _ ->
                                -- done, last playlist was empty
                                refresh model

                    else
                        ( model
                        , Api.appendToPlaylist model.token playlistId currentTracks <|
                            TracksAdded done playlistId todo remainingTracks
                        )

        Export playlist ->
            ( model, Api.fetchTracks model.token playlist <| GotTracks playlist [] )


coverColumn : Column Playlist BackupMsg
coverColumn =
    { header = heading "Cover image"
    , width = shrink
    , view =
        \{ images } ->
            List.head images
                |> Maybe.map
                    (\img ->
                        image
                            [ width <| px 64
                            , height <| px 64
                            ]
                            { src = img.url, description = "" }
                    )
                |> Maybe.withDefault (text "")
    }


importNameColumn : Column Backup.Playlist BackupMsg
importNameColumn =
    { header = heading "Name"
    , width = shrink
    , view =
        \{ originalUrl, name } ->
            el [ centerY, alignLeft, paddingEach { edges | right = 10 } ] <|
                newTabLink [ Font.underline ]
                    { url = originalUrl
                    , label = text name
                    }
    }


importRenameColumn : ImportModel -> Column Backup.Playlist BackupMsg
importRenameColumn { existing, renames } =
    { header = heading "Rename to"
    , width = shrink
    , view =
        \{ name } ->
            let
                rename =
                    Maybe.withDefault name <| Dict.get name renames

                trimmed =
                    String.trim rename
            in
            Input.text
                [ Font.color black
                , if Set.member trimmed existing then
                    Background.color lightRed

                  else
                    Background.color white
                ]
                { onChange = RenameImport name
                , text = rename
                , placeholder = Nothing
                , label = labelHidden name
                }
    }


importImportColumn : ImportModel -> Column Backup.Playlist BackupMsg
importImportColumn { playlists, selectedPlaylists } =
    { header =
        headingRow
            [ text "Import"
            , checkbox [ centerY ]
                { onChange = SelectAllForImport
                , icon = defaultCheckbox
                , checked = List.all (\{ originalId } -> Set.member originalId selectedPlaylists) playlists
                , label =
                    labelLeft
                        [ Font.size 12
                        , paddingEach { edges | left = 10 }
                        , centerY
                        ]
                    <|
                        text "(all)"
                }
            ]
    , width = shrink
    , view =
        \playlist ->
            row [ centerY ]
                [ checkbox [ centerY, paddingEach { edges | left = 10 } ]
                    { onChange = SelectForImport playlist.originalId
                    , icon = defaultCheckbox
                    , checked = Set.member playlist.originalId selectedPlaylists
                    , label = labelHidden playlist.originalId
                    }
                ]
    }


nameColumn : Column Playlist BackupMsg
nameColumn =
    { header = heading "Name"
    , width = fill
    , view =
        \{ url, name } ->
            el [ centerY, alignLeft, paddingEach { edges | right = 10 } ] <|
                newTabLink [ Font.underline ]
                    { url = url
                    , label = text name
                    }
    }


ownerColumn : Column Playlist BackupMsg
ownerColumn =
    { header = heading "Owner"
    , width = shrink
    , view =
        \{ owner } ->
            el [ centerY, paddingEach { edges | right = 10 } ] <|
                newTabLink [ Font.underline ]
                    { url = owner.url
                    , label = text <| Maybe.withDefault owner.id owner.displayName
                    }
    }


visibilityColumn : Column Playlist BackupMsg
visibilityColumn =
    { header = heading "Visibility"
    , width = shrink
    , view =
        \{ isPublic } ->
            el [ centerY, paddingEach { edges | right = 10 } ] <|
                text <|
                    visibilityToString isPublic
    }


tracksColumn : Column Playlist BackupMsg
tracksColumn =
    { header = heading "#Tracks"
    , width = shrink
    , view =
        \{ tracks } ->
            el [ centerY, alignRight ] <|
                text <|
                    String.fromInt tracks.total
    }


exportColumn : BackupModel -> Column Playlist BackupMsg
exportColumn { playlists, selectedPlaylists, status } =
    { header =
        headingRow
            [ text "Export"
            , checkbox [ centerY ]
                { onChange = SelectAll
                , icon = defaultCheckbox
                , checked = List.all (\{ id } -> Set.member id selectedPlaylists) playlists
                , label =
                    labelLeft
                        [ paddingEach { edges | left = 10 }
                        , Font.size 12
                        , centerY
                        ]
                    <|
                        text "(all)"
                }
            ]
    , width = shrink
    , view =
        \playlist ->
            row []
                [ checkbox [ centerY, paddingEach { edges | right = 10 } ]
                    { onChange = PlaylistSelected playlist.id
                    , icon = defaultCheckbox
                    , checked = Set.member playlist.id selectedPlaylists
                    , label = labelHidden playlist.id
                    }
                , case status of
                    Nothing ->
                        spotifyButton "Export this." <| Just <| Export playlist

                    Just _ ->
                        disabledButton "Export this."
                ]
    }


actionButtons : BackupModel -> Element BackupMsg
actionButtons { status } =
    let
        buttons =
            [ ( "Refresh playlists.", Enter )
            , ( "Export selected.", ExportSelected )
            , ( "Export all.", ExportAll )
            , ( "Import.", Import )
            ]
    in
    row [ centerX ]
        (case status of
            Nothing ->
                List.map (\( txt, msg ) -> spotifyButton txt <| Just msg) buttons

            Just _ ->
                List.map (\( txt, _ ) -> disabledButton txt) buttons
        )


view : BackupModel -> Html BackupMsg
view model =
    layout
        [ Background.color spotifyBackground
        , Font.color spotifyForeground
        , inFront <| importDialog model
        ]
    <|
        column [ padding 20, width fill ]
            [ Maybe.withDefault none <| Maybe.map (\err -> text <| "Error: " ++ err) model.error
            , Maybe.withDefault none <| Maybe.map (\stat -> text <| "Status: " ++ stat) model.status
            , actionButtons model
            , table []
                { data = model.playlists
                , columns =
                    [ coverColumn
                    , nameColumn
                    , ownerColumn
                    , visibilityColumn
                    , tracksColumn
                    , exportColumn model
                    ]
                }
            , actionButtons model
            ]
