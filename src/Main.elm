module Main exposing (main)

import Backup exposing (BackupModel, BackupMsg(..))
import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation as Nav exposing (Key)
import Element exposing (Element, centerX)
import Element.Font as Font
import Html exposing (Html, div)
import Route exposing (Route(..))
import Spotify.Scope as Scope exposing (Scope(..))
import Spotify.Token as Token exposing (Token)
import Style exposing (centeredBody, spotifyButton)
import Url exposing (Url)


type alias Model =
    { key : Key
    , route : Route
    , backup : Maybe BackupModel
    }


type Msg
    = Redirect UrlRequest
    | Navigated Url
    | BackupMessage BackupMsg


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { key = key
            , route = Route.fromUrl url
            , backup = Nothing
            }
    in
    case model.route of
        Callback error fragment ->
            let
                maybeToken =
                    Maybe.map Token.fromFragment fragment
            in
            case ( error, maybeToken ) of
                ( Nothing, Just token ) ->
                    ( { model | backup = Maybe.map Backup.init token }, Nav.replaceUrl key "/backup" )

                _ ->
                    ( model, Nav.load "/" )

        Home ->
            ( model, Cmd.none )

        _ ->
            ( model, Nav.load "/" )


viewNotFoundPage : Url -> Html Msg
viewNotFoundPage url =
    centeredBody
        [ Element.text <| "The path '" ++ url.path ++ "' does not exist."
        , Element.link [ centerX, Font.underline ] { url = "/", label = Element.text "Go to start page." }
        ]


redirectToExternal : String -> Msg
redirectToExternal urlString =
    Redirect <| External urlString


scopes : List Scope -> String
scopes scopeList =
    List.map Scope.toString scopeList
        |> String.join " "
        |> Url.percentEncode


viewStartPage : Html Msg
viewStartPage =
    centeredBody
        [ spotifyButton "Login to Spotify." <|
            Just
                (redirectToExternal
                    ("https://accounts.spotify.com/authorize"
                        ++ "?client_id=d09a22d09df145d4bd86d541fd46f4b4"
                        ++ "&response_type=token"
                        ++ "&redirect_uri="
                        ++ Url.percentEncode "https://spotify-backup.drako.guru/callback"
                        ++ "&show_dialog=true"
                        ++ "&scope="
                        ++ scopes Scope.all
                    )
                )
        ]


view : Model -> Document Msg
view model =
    { title = "Spotify Backup"
    , body =
        [ case ( model.route, model.backup ) of
            ( Home, _ ) ->
                viewStartPage

            ( NotFound url, _ ) ->
                viewNotFoundPage url

            ( Backup, Just backupModel ) ->
                Html.map BackupMessage <| Backup.view backupModel

            _ ->
                -- this would never happen
                div [] []
        ]
    }


maybeModel : ( model, msg ) -> ( Maybe model, msg )
maybeModel ( model, msg ) =
    ( Just model, msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.backup ) of
        ( Navigated url, _ ) ->
            let
                route =
                    Route.fromUrl url

                ( backupModel, backupMsg ) =
                    case ( route, model.backup ) of
                        ( Backup, Just oldBackupModel ) ->
                            maybeModel <| Backup.update Enter oldBackupModel

                        _ ->
                            ( model.backup, Cmd.none )
            in
            ( { model | route = Route.fromUrl url, backup = backupModel }, Cmd.map BackupMessage backupMsg )

        ( Redirect urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                External url ->
                    ( model, Nav.load url )

        ( BackupMessage backupMsg, Just backupModel ) ->
            let
                ( updatedModel, cmd ) =
                    Backup.update backupMsg backupModel
            in
            ( { model | backup = Just updatedModel }, Cmd.map BackupMessage cmd )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Redirect
        , onUrlChange = Navigated
        }
