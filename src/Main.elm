module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation as Nav exposing (Key)
import Element exposing (Element, centerX, centerY, padding, rgb255, spacing)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, div)
import Route exposing (Route(..))
import Spotify.Scope as Scope exposing (Scope(..))
import Spotify.Token as Token exposing (Token)
import Url exposing (Url)


type alias Model =
    { key : Key
    , route : Route
    , token : Maybe Token
    }


type Msg
    = Redirect UrlRequest
    | Navigated Url


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { key = key
            , route = Route.fromUrl url
            , token = Nothing
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
                    ( { model | token = token }, Nav.replaceUrl key "/backup" )

                _ ->
                    ( model, Nav.load "/" )

        _ ->
            ( model, Cmd.none )


centeredBody : List (Element Msg) -> Html Msg
centeredBody elements =
    Element.layout [] <|
        Element.column [ centerX, centerY, spacing 15 ] elements


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
        [ Input.button
            [ Border.solid
            , Border.rounded 10
            , Border.color (rgb255 0x00 0x00 0xFF)
            , Border.width 2
            , padding 10
            ]
            { onPress =
                Just
                    (redirectToExternal
                        ("https://accounts.spotify.com/authorize"
                            ++ "?client_id=d09a22d09df145d4bd86d541fd46f4b4"
                            ++ "&response_type=token"
                            ++ "&redirect_uri="
                            ++ Url.percentEncode "https://spotify-backup.drako.guru/callback"
                            ++ "&show_dialog=true"
                            ++ "&scope="
                            ++ scopes [ PlaylistReadPrivate, PlaylistModifyPrivate, PlaylistModifyPublic ]
                        )
                    )
            , label = Element.text "Login to Spotify."
            }
        ]


view : Model -> Document Msg
view model =
    { title = "Spotify Backup"
    , body =
        [ case model.route of
            Home ->
                viewStartPage

            NotFound url ->
                viewNotFoundPage url

            _ ->
                -- this would never happen
                div [] []
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigated url ->
            ( { model | route = Route.fromUrl url }, Cmd.none )

        Redirect urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                External url ->
                    ( model, Nav.load url )


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
