module Pages.Backup exposing (BackupModel, BackupMsg(..), init, update, view)

import Backup.Encoder exposing (playlistToJson)
import Backup.Payloads exposing (spotifyToBackup)
import Element exposing (Column, Element, alignLeft, alignRight, centerY, column, el, fill, height, image, layout, newTabLink, none, padding, paddingEach, px, row, shrink, table, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (checkbox, defaultCheckbox, labelHidden, labelLeft)
import File.Download as Download
import Html exposing (Html)
import Http exposing (Error)
import Set exposing (Set)
import Spotify.Api as Api exposing (errorToString)
import Spotify.Payloads exposing (Paging, Playlist, Track, visibilityToString)
import Spotify.Token exposing (Token)
import Style exposing (edges, heading, headingRow, spotifyBackground, spotifyButton, spotifyForeground)


type BackupMsg
    = Enter
    | GotPlaylists (Result Error (Paging Playlist))
    | PlaylistSelected String Bool
    | SelectAll Bool
    | Export Playlist
    | GotTracks Playlist (Result Error (Paging Track))


type alias BackupModel =
    { token : Token
    , error : Maybe String
    , playlists : List Playlist
    , selectedPlaylists : Set String
    }


init : Token -> BackupModel
init tok =
    { playlists = []
    , selectedPlaylists = Set.empty
    , error = Nothing
    , token = tok
    }


update : BackupMsg -> BackupModel -> ( BackupModel, Cmd BackupMsg )
update msg model =
    case msg of
        Enter ->
            ( { model | playlists = [] }, Api.fetchPlaylists model.token GotPlaylists )

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
                    ( { model | playlists = model.playlists ++ playlists.items }
                    , Api.fetchMorePlaylists model.token playlists GotPlaylists
                    )

                Err error ->
                    ( { model | error = Just ("Failed retrieving Playlists: " ++ errorToString error) }, Cmd.none )

        GotTracks playlist result ->
            case result of
                Ok tracks ->
                    ( model
                    , Download.string "playlist.json" "application/json" <|
                        playlistToJson <|
                            spotifyToBackup playlist tracks.items
                    )

                Err error ->
                    ( { model | error = Just ("Failed retrieving Tracks: " ++ errorToString error) }, Cmd.none )

        Export playlist ->
            ( model, Api.fetchTracks model.token playlist <| GotTracks playlist )


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
    , width = shrink
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
exportColumn { playlists, selectedPlaylists } =
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
                , spotifyButton "Export this." <| Just <| Export playlist
                ]
    }


view : BackupModel -> Html BackupMsg
view model =
    layout
        [ Background.color spotifyBackground
        , Font.color spotifyForeground
        ]
    <|
        column [ padding 20 ]
            [ Maybe.withDefault none <| Maybe.map text model.error
            , table [ width fill ]
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
            ]
