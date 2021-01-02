module Pages.Backup exposing (BackupModel, BackupMsg(..), init, update, view)

import Backup.Encoder exposing (playlistToJson, playlistsToJson)
import Backup.Payloads as Backup exposing (spotifyToBackup)
import Char exposing (isAlphaNum)
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
        , layout
        , newTabLink
        , none
        , padding
        , paddingEach
        , px
        , row
        , shrink
        , table
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (checkbox, defaultCheckbox, labelHidden, labelLeft)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html)
import Http exposing (Error)
import List exposing (reverse)
import Set exposing (Set)
import Spotify.Api as Api exposing (errorToString)
import Spotify.Payloads exposing (Paging, Playlist, Track, visibilityToString)
import Spotify.Token exposing (Token)
import Style exposing (disabledButton, edges, heading, headingRow, spotifyBackground, spotifyButton, spotifyForeground)
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


type alias BackupModel =
    { token : Token
    , error : Maybe String
    , status : Maybe String
    , playlists : List Playlist
    , selectedPlaylists : Set String
    }


init : Token -> BackupModel
init tok =
    { playlists = []
    , selectedPlaylists = Set.empty
    , error = Nothing
    , status = Nothing
    , token = tok
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


progress : List Backup.Playlist -> List Playlist -> String
progress done todo =
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


retrievalFailure : BackupModel -> String -> Error -> ( BackupModel, Cmd BackupMsg )
retrievalFailure model what err =
    ( { model
        | status = Nothing
        , error = Just <| "Failed retrieving " ++ what ++ ": " ++ errorToString err
      }
    , Cmd.none
    )


update : BackupMsg -> BackupModel -> ( BackupModel, Cmd BackupMsg )
update msg model =
    case msg of
        Enter ->
            ( { model | playlists = [], status = Just "Retrieving playlists." }
            , Api.fetchPlaylists model.token GotPlaylists
            )

        Import ->
            ( model, Select.file [ "application/json" ] ImportFileSelected )

        ImportFileSelected file ->
            ( { model | status = Just <| "Reading file: " ++ File.name file ++ "..." }
            , Task.perform ImportFileLoaded (File.toString file)
            )

        ImportFileLoaded _ ->
            ( { model | status = Nothing }, Cmd.none )

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
                                    ( { model | status = Just <| progress newDone remaining }
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
                    ( { model | status = Just <| progress [] remainingPlaylists }
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
                    ( { model | status = Just <| progress [] remainingPlaylists }
                    , Api.fetchTracks model.token pl <| GotTracksMultiPlaylist [] pl remainingPlaylists []
                    )

                _ ->
                    -- no playlists selected
                    ( model, Cmd.none )

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
