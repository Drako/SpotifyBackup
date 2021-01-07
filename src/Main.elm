module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html)
import Http exposing (Error)
import Pages.Backup as Backup exposing (BackupModel, BackupMsg(..))
import Route exposing (Route(..))
import Spotify.Api as Api
import Spotify.Scope as Scope exposing (Scope(..))
import Spotify.Token as Token exposing (Token)
import Style exposing (centeredBody, spotifyButton)
import Url exposing (Url)


type Page
    = StartPage
    | BackupPage BackupModel


type alias Model =
    { key : Key
    , page : Page
    }


type Msg
    = Redirect UrlRequest
    | Navigated Url
    | BackupMessage BackupMsg
    | ReceivedUser Token (Result Error String)


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Route.fromUrl url

        model =
            { key = key
            , page = StartPage
            }
    in
    case route of
        Callback error fragment ->
            let
                maybeToken =
                    fragment
                        |> Maybe.andThen Token.fromFragment
            in
            case ( error, maybeToken ) of
                ( Nothing, Just token ) ->
                    ( model, Api.fetchUserId token <| ReceivedUser token )

                _ ->
                    ( model, Nav.load "/" )

        Home ->
            ( model, Cmd.none )

        _ ->
            ( model, Nav.load "/" )


redirectToExternal : String -> Msg
redirectToExternal urlString =
    Redirect <| External urlString


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
                        ++ Scope.toQueryParams Scope.all
                    )
                )
        ]


view : Model -> Document Msg
view model =
    { title = "Spotify Backup"
    , body =
        [ case model.page of
            StartPage ->
                viewStartPage

            BackupPage backupModel ->
                Html.map BackupMessage <| Backup.view backupModel
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ReceivedUser token result, _ ) ->
            case result of
                Ok userId ->
                    ( { model | page = BackupPage <| Backup.init token userId }, Nav.replaceUrl model.key "/backup" )

                Err _ ->
                    ( model, Nav.load "/" )

        ( Navigated url, _ ) ->
            let
                route =
                    Route.fromUrl url

                ( page, cmd ) =
                    case ( route, model.page ) of
                        -- special behaviour when entering /backup via redirect
                        ( Backup, BackupPage oldBackupModel ) ->
                            Backup.update Enter oldBackupModel
                                |> (\( a, b ) -> ( BackupPage a, Cmd.map BackupMessage b ))

                        _ ->
                            ( model.page, Cmd.none )
            in
            ( { model | page = page }, cmd )

        ( Redirect urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                External url ->
                    ( model, Nav.load url )

        ( BackupMessage backupMsg, BackupPage backupModel ) ->
            let
                ( updatedModel, cmd ) =
                    Backup.update backupMsg backupModel
            in
            ( { model | page = BackupPage updatedModel }, Cmd.map BackupMessage cmd )

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
