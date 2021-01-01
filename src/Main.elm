module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation as Nav exposing (Key)
import Element exposing (Element, centerX)
import Element.Font as Font
import Html exposing (Html, div)
import Pages.Backup as Backup exposing (BackupModel, BackupMsg(..))
import Route exposing (Route(..))
import Spotify.Scope as Scope exposing (Scope(..))
import Spotify.Token as Token exposing (Token)
import Style exposing (centeredBody, spotifyButton)
import Url exposing (Url)


type Page
    = StartPage
    | BackupPage BackupModel


type alias Model =
    { key : Key
    , route : Route
    , page : Page
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
            , page = StartPage
            }
    in
    case model.route of
        Callback error fragment ->
            let
                maybeToken =
                    fragment
                        |> Maybe.andThen Token.fromFragment
            in
            case ( error, maybeToken ) of
                ( Nothing, Just token ) ->
                    ( { model | page = BackupPage <| Backup.init token }, Nav.replaceUrl key "/backup" )

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
        [ case ( model.route, model.page ) of
            ( Home, StartPage ) ->
                viewStartPage

            ( NotFound url, _ ) ->
                viewNotFoundPage url

            ( Backup, BackupPage backupModel ) ->
                Html.map BackupMessage <| Backup.view backupModel

            _ ->
                -- this would never happen
                div [] []
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
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
            ( { model | route = Route.fromUrl url, page = page }, cmd )

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
