module Main exposing (Model, Msg(..), Page(..), init, main, subscriptions, update, view)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html)
import Http exposing (Error)
import Pages.Backup as Backup exposing (BackupModel, BackupMsg(..))
import Pages.Home as Home exposing (HomeMsg(..))
import Route exposing (Route(..))
import Spotify.Api as Api
import Spotify.Token as Token exposing (Token)
import Url exposing (Url)


type Page
    = HomePage
    | BackupPage BackupModel


type alias Model =
    { key : Maybe Key
    , page : Page
    }


type Msg
    = Redirect UrlRequest
    | Navigated Url
    | HomeMessage HomeMsg
    | BackupMessage BackupMsg
    | ReceivedUser Token (Result Error String)


init : Url -> Maybe Key -> ( Model, Cmd Msg )
init url key =
    let
        route =
            Route.fromUrl url

        model =
            { key = key
            , page = HomePage
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


view : (() -> Html HomeMsg) -> (BackupModel -> Html BackupMsg) -> Model -> Document Msg
view homeView backupView model =
    { title = "Spotify Backup"
    , body =
        [ case model.page of
            HomePage ->
                Html.map HomeMessage (homeView ())

            BackupPage backupModel ->
                Html.map BackupMessage <| backupView backupModel
        ]
    }


withKey : (Key -> Cmd Msg) -> Maybe Key -> Cmd Msg
withKey cmd =
    Maybe.map cmd >> Maybe.withDefault Cmd.none


redirect : Model -> UrlRequest -> ( Model, Cmd Msg )
redirect model urlRequest =
    case urlRequest of
        Internal url ->
            ( model, model.key |> withKey (\key -> Nav.pushUrl key <| Url.toString url) )

        External url ->
            ( model, Nav.load url )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ReceivedUser token result, _ ) ->
            case result of
                Ok userId ->
                    ( { model | page = BackupPage <| Backup.init token userId }
                    , model.key |> withKey (\key -> Nav.replaceUrl key "/backup")
                    )

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
            redirect model urlRequest

        ( HomeMessage (Authenticate urlRequest), _ ) ->
            redirect model urlRequest

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
        { init = \_ url key -> init url (Just key)
        , view = view (\_ -> Home.view) Backup.view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Redirect
        , onUrlChange = Navigated
        }
