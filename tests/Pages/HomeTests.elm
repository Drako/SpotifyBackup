module Pages.HomeTests exposing (..)

import Browser exposing (UrlRequest(..))
import Pages.Home exposing (HomeMsg(..), view)
import Test exposing (Test, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector, class, containing, tag, text)


loginButton : List Selector
loginButton =
    [ class "sbt", containing [ text "Login to Spotify." ] ]


spotifyLoginUrl : String
spotifyLoginUrl =
    "https://accounts.spotify.com/authorize?client_id=d09a22d09df145d4bd86d541fd46f4b4&response_type=token"
        ++ "&redirect_uri=https%3A%2F%2Fspotify-backup.drako.guru%2Fcallback&show_dialog=true"
        ++ "&scope=playlist-read-private%20playlist-modify-private%20playlist-modify-public%20playlist-read-collaborative"


containsLoginButtonTest : Test
containsLoginButtonTest =
    test "[view] Home page should contain a login button for Spotify SSO" <|
        \_ ->
            Query.fromHtml view
                |> Query.find loginButton
                |> Event.simulate Event.click
                |> Event.expect (Authenticate <| External spotifyLoginUrl)


containsGithubLink : Test
containsGithubLink =
    test "[view] Home page should contain link to GitHub repository" <|
        \_ ->
            Query.fromHtml view
                |> Query.find [ tag "a" ]
                |> Query.has [ text "Source on GitHub" ]
