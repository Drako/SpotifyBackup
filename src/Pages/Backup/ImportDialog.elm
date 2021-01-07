module Pages.Backup.ImportDialog exposing (ImportModel, ImportMsg(..), update, view)

import Backup.Payloads as Backup
import Dialog
import Dict exposing (Dict)
import Element
    exposing
        ( Column
        , Element
        , alignLeft
        , centerX
        , centerY
        , el
        , fill
        , height
        , maximum
        , newTabLink
        , padding
        , paddingEach
        , row
        , scrollbarY
        , shrink
        , table
        , text
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (checkbox, defaultCheckbox, labelHidden, labelLeft)
import Element.Region as Region
import Set exposing (Set)
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
        , white
        )


type alias ImportModel =
    { playlists : List Backup.Playlist
    , renames : Dict String String
    , selectedPlaylists : Set String
    , existing : Set String
    }


type ImportMsg
    = CloseImport
    | RenameImport String String
    | SelectAllForImport Bool
    | SelectNonCollidingForImport
    | SelectForImport String Bool
    | ImportSelected


nonColliding : ImportModel -> Set String
nonColliding { renames, existing, playlists } =
    playlists
        |> List.filterMap
            (\{ originalId, name } ->
                let
                    renamed =
                        Dict.get name renames |> Maybe.map String.trim |> Maybe.withDefault name
                in
                if Set.member renamed existing then
                    Nothing

                else
                    Just originalId
            )
        |> Set.fromList


noCollisions : ImportModel -> Bool
noCollisions { renames, existing, selectedPlaylists, playlists } =
    playlists
        |> List.filter (\{ originalId } -> Set.member originalId selectedPlaylists)
        |> List.filterMap (\{ name } -> Dict.get name renames)
        |> List.map String.trim
        |> List.any (\name -> Set.member name existing)
        |> not


nameColumn : Column Backup.Playlist ImportMsg
nameColumn =
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


renameColumn : ImportModel -> Column Backup.Playlist ImportMsg
renameColumn { existing, renames } =
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


importColumn : ImportModel -> Column Backup.Playlist ImportMsg
importColumn { playlists, selectedPlaylists } =
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


update : ImportMsg -> Maybe ImportModel -> ( Maybe ImportModel, Cmd ImportMsg )
update msg model =
    case model of
        Nothing ->
            ( Nothing, Cmd.none )

        Just importModel ->
            case msg of
                SelectForImport id selected ->
                    if selected then
                        ( Just { importModel | selectedPlaylists = Set.insert id importModel.selectedPlaylists }
                        , Cmd.none
                        )

                    else
                        ( Just { importModel | selectedPlaylists = Set.remove id importModel.selectedPlaylists }
                        , Cmd.none
                        )

                SelectAllForImport selected ->
                    if selected then
                        ( Just { importModel | selectedPlaylists = Set.fromList <| List.map .originalId importModel.playlists }
                        , Cmd.none
                        )

                    else
                        ( Just { importModel | selectedPlaylists = Set.empty }
                        , Cmd.none
                        )

                SelectNonCollidingForImport ->
                    ( Just { importModel | selectedPlaylists = nonColliding importModel }, Cmd.none )

                RenameImport name rename ->
                    ( Just { importModel | renames = Dict.insert name rename importModel.renames }
                    , Cmd.none
                    )

                _ ->
                    -- ImportSelected and CloseImport are handled in the Backup page
                    -- the ImportDialog is only responsible for the selection process
                    ( Just importModel, Cmd.none )


view : Maybe ImportModel -> Element ImportMsg
view model =
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
                            [ height <| maximum 600 fill
                            , scrollbarY
                            ]
                        <|
                            table []
                                { data = importModel.playlists
                                , columns =
                                    [ nameColumn
                                    , renameColumn importModel
                                    , importColumn importModel
                                    ]
                                }
                , footer =
                    Just <|
                        row [ centerX ]
                            [ spotifyButton "Select non-colliding." <| Just SelectNonCollidingForImport
                            , if (not <| Set.isEmpty importModel.selectedPlaylists) && noCollisions importModel then
                                spotifyButton "Import." <| Just ImportSelected

                              else
                                disabledButton "Import."
                            ]
                }
            )
            model
