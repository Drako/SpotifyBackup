module Backup exposing (..)

import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, fill, height, image, layout, newTabLink, padding, paddingEach, px, shrink, table, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (checkbox, defaultCheckbox, labelHidden)
import Html exposing (Html)
import Http exposing (Error)
import Set exposing (Set)
import Spotify.Api as Api exposing (errorToString)
import Spotify.Payloads exposing (Paging, Playlist)
import Spotify.Token exposing (Token)
import Style exposing (edges, heading, spotifyBackground, spotifyForeground)


type BackupMsg
    = Enter
    | GotPlaylists (Result Error (Paging Playlist))
    | PlaylistSelected String Bool


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

        PlaylistSelected id value ->
            if value == True then
                ( { model | selectedPlaylists = Set.insert id model.selectedPlaylists }, Cmd.none )

            else
                ( { model | selectedPlaylists = Set.remove id model.selectedPlaylists }, Cmd.none )

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
    layout
        [ Background.color spotifyBackground
        , Font.color spotifyForeground
        ]
    <|
        column [ padding 20 ]
            [ text <| Maybe.withDefault "" model.error
            , table [ width fill ]
                { data = model.playlists
                , columns =
                    [ { header = heading "Cover image"
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
                    , { header = heading "Name"
                      , width = shrink
                      , view =
                            \{ name } ->
                                el [ centerY, alignLeft, paddingEach { edges | right = 10 } ] <|
                                    text name
                      }
                    , { header = heading "Owner"
                      , width = shrink
                      , view =
                            \{ owner } ->
                                el [ centerY, paddingEach { edges | right = 10 } ] <|
                                    newTabLink [ Font.underline ]
                                        { url = owner.url
                                        , label = text <| Maybe.withDefault owner.id owner.displayName
                                        }
                      }
                    , { header = heading "Visibility"
                      , width = shrink
                      , view =
                            \{ isPublic } ->
                                el [ centerY, paddingEach { edges | right = 10 } ] <|
                                    text <|
                                        if isPublic then
                                            "public"

                                        else
                                            "private"
                      }
                    , { header = heading "#Tracks"
                      , width = shrink
                      , view =
                            \{ tracks } ->
                                el [ centerY, alignRight ] <|
                                    text <|
                                        String.fromInt tracks.total
                      }
                    , { header = heading "Export"
                      , width = shrink
                      , view =
                            \{ id } ->
                                checkbox [ centerY ]
                                    { onChange = PlaylistSelected id
                                    , icon = defaultCheckbox
                                    , checked = Set.member id model.selectedPlaylists
                                    , label = labelHidden id
                                    }
                      }
                    ]
                }
            ]
