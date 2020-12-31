module Backup exposing (..)

import Element exposing (Element, centerX, column, fill, layout, spacing, table, text)
import Html exposing (Html)
import Http exposing (Error)
import Spotify.Api as Api exposing (errorToString)
import Spotify.Payloads exposing (Paging, Playlist)
import Spotify.Token exposing (Token)


type BackupMsg
    = Enter
    | GotPlaylists (Result Error (Paging Playlist))


type alias BackupModel =
    { token : Token
    , error : Maybe String
    , playlists : List Playlist
    }


init : Token -> BackupModel
init tok =
    { playlists = []
    , error = Nothing
    , token = tok
    }


update : BackupMsg -> BackupModel -> ( BackupModel, Cmd BackupMsg )
update msg model =
    case msg of
        Enter ->
            ( { model | playlists = [] }, Api.fetchPlaylists model.token GotPlaylists )

        GotPlaylists result ->
            case result of
                Ok playlists ->
                    ( { model | playlists = model.playlists ++ playlists.items }
                    , Api.fetchMorePlaylists model.token playlists GotPlaylists
                    )

                Err error ->
                    ( { model | error = Just ("Failed retrieving Playlists: " ++ errorToString error) }, Cmd.none )


view : BackupModel -> Html BackupMsg
view model =
    layout [] <|
        column []
            [ text <| Maybe.withDefault "" model.error
            , table [ centerX, spacing 10 ]
                { data = model.playlists
                , columns =
                    [ { header = text "ID", width = fill, view = \playlist -> text playlist.id }
                    , { header = text "Name", width = fill, view = \playlist -> text playlist.name }
                    , { header = text "#Tracks"
                      , width = fill
                      , view =
                            \playlist ->
                                text <|
                                    String.fromInt playlist.tracks.total
                      }
                    ]
                }
            ]
