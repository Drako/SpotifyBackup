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
        , edges
        , headingRow
        , headingText
        , lightRed
        , spotifyBackground
        , spotifyButton
        , white
        )
import Utilities exposing (ifBlank)


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
                        Dict.get originalId renames |> Maybe.map String.trim |> Maybe.withDefault name
                in
                if Set.member renamed existing || String.isEmpty renamed then
                    Nothing

                else
                    Just originalId
            )
        |> Set.fromList


noCollisions : ImportModel -> Bool
noCollisions { renames, existing, selectedPlaylists, playlists } =
    -- TODO: make sure that renamed playlists also don't collide with each other
    playlists
        |> List.filter (\{ originalId } -> Set.member originalId selectedPlaylists)
        |> List.map (\{ originalId, name } -> Dict.get originalId renames |> Maybe.withDefault name)
        |> List.map String.trim
        |> List.any (\name -> Set.member name existing || String.isEmpty name)
        |> not


nameColumn : Column Backup.Playlist ImportMsg
nameColumn =
    { header = headingText "Name"
    , width = shrink
    , view =
        \{ originalUrl, name } ->
            el [ centerY, alignLeft, paddingEach { edges | right = 10 } ] <|
                newTabLink [ Font.underline ]
                    { url = originalUrl
                    , label = text <| ifBlank "<blank>" name
                    }
    }


renameColumn : ImportModel -> Column Backup.Playlist ImportMsg
renameColumn { existing, renames } =
    { header = headingText "Rename to"
    , width = shrink
    , view =
        \{ originalId, name } ->
            let
                rename =
                    Maybe.withDefault name <| Dict.get originalId renames

                trimmed =
                    String.trim rename
            in
            Input.text
                [ Font.color black
                , if Set.member trimmed existing || String.isEmpty trimmed then
                    Background.color lightRed

                  else
                    Background.color white
                ]
                { onChange = RenameImport originalId
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

                RenameImport originalId rename ->
                    ( Just { importModel | renames = Dict.insert originalId rename importModel.renames }
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
                            [ spotifyButton "Select non-colliding." True SelectNonCollidingForImport
                            , let
                                importEnabled : Bool
                                importEnabled =
                                    (not <| Set.isEmpty importModel.selectedPlaylists) && noCollisions importModel
                              in
                              spotifyButton "Import." importEnabled ImportSelected
                            ]
                }
            )
            model
