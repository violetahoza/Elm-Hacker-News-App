module Main exposing (devFlags, init, main, prodFlags, reactorMain, update, view)

import Browser
import Dict exposing (update)
import Effect exposing (Effect, performEffect)
import Html exposing (Html, button, div, input, select, text)
import Html.Attributes exposing (href, type_)
import Model exposing (AppState(..), Config, LoadingPostsState, Mode(..), Model, Msg(..))
import Model.Post as Post
import Model.PostIds as PostIds exposing (HackerNewsItem(..))
import Model.PostsConfig
import View.Posts exposing (postTable, postsConfigView)
import Model.PostsConfig exposing (applyChanges)
import Model.PostsConfig exposing (filterPosts)
import Html.Attributes exposing (style)


prodFlags : Config
prodFlags =
    { apiUrl = "https://hacker-news.firebaseio.com", mode = Prod }


devFlags : Config
devFlags =
    { apiUrl = "http://localhost:3000", mode = Dev }


{-| Create a program that uses the "production" configuration (uses the real hackernews API) to fetch real data from HackerNews.
-}
main : Program () Model Msg
main =
    Browser.element -- initialize the app
        { init = \flags -> init prodFlags flags |> Tuple.mapSecond performEffect -- initialize the model with the flags
        , view = view -- render the view
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect -- update the model with the msg
        , subscriptions = subscriptions -- handle subscriptions
        }


{-| Create a program that uses the development configuration (uses a local server that returns hardcoded hackernews posts)
-}
reactorMain : Program () Model Msg
reactorMain =
    Browser.element
        { init = \flags -> init devFlags flags |> Tuple.mapSecond performEffect
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffect
        , subscriptions = subscriptions
        }


{-| Don't modify
-}
init : Config -> () -> ( Model, Effect ) -- initialize the model (AppState) with a configuration (Config)
init flags _ =
    ( Model.initModel flags
    , Effect.GetTime -- triggers an effect to get the current time
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- Fetches post IDs for a specific HackerNews category (ex: Top, New).
getItems : String -> HackerNewsItem -> Effect
getItems apiUrl item =
    Effect.GetItems { apiUrl = apiUrl, item = item, onResult = GotPostIds, decoder = PostIds.decode }

-- Fetches the top post IDs by calling getItems with the Top category.
getTopPostIds : String -> Effect
getTopPostIds apiUrl =
    getItems apiUrl Top

--  Fetches a specific post using its ID and decodes it into a Post structure.
getPost : String -> Int -> Effect
getPost apiUrl postId =
    Effect.GetPost { apiUrl = apiUrl, postId = postId, onResult = GotPost, decoder = Post.decode }


addLoadedPost : Post.Post -> LoadingPostsState -> LoadingPostsState
addLoadedPost post state =
    { state | posts = post :: state.posts }


update : Msg -> Model -> ( Model, Effect )
update msg model =
    let
        ( newState, cmd ) =
            case ( model.state, msg ) of
                ( Model.Empty { config }, GotTime time ) ->
                    ( Model.Loading { config = config, time = time }, getTopPostIds model.config.apiUrl )

                ( Model.Loading { config, time }, GotPostIds result ) ->
                    case result of
                        Ok (Just ids) ->
                            ( Model.LoadingPosts
                                { config = config
                                , time = time
                                , postIds = ids
                                , currentId = PostIds.first ids
                                , posts = []
                                }
                            , getPost model.config.apiUrl (PostIds.first ids)
                            )

                        Ok Nothing ->
                            ( Model.Empty { config = config }, Effect.NoEffect )

                        Err err ->
                            ( Model.FailedToLoad err, Effect.NoEffect )

                ( Model.LoadingPosts loading, GotPost result ) ->
                    case result of
                        Ok post ->
                            case PostIds.advance loading.postIds of
                                Just ( nextId, nextPostIds ) ->
                                    let
                                        posts =
                                            post :: loading.posts
                                    in
                                    if List.length posts < loading.config.postsToFetch then
                                        ( Model.LoadingPosts
                                            { loading
                                                | postIds = nextPostIds
                                                , currentId = nextId
                                                , posts = posts
                                            }
                                        , getPost model.config.apiUrl nextId
                                        )

                                    else
                                        ( Model.LoadedPosts
                                            { config = loading.config
                                            , time = loading.time
                                            , posts = List.reverse (post :: loading.posts)
                                            }
                                        , Effect.NoEffect
                                        )

                                Nothing ->
                                    ( Model.LoadedPosts
                                        { config = loading.config
                                        , time = loading.time
                                        , posts = List.reverse (post :: loading.posts)
                                        }
                                    , Effect.NoEffect
                                    )

                        Err err ->
                            ( Model.FailedToLoad err, Effect.NoEffect )

                ( Model.LoadedPosts state, ConfigChanged change ) ->
                    -- ( Model.LoadedPosts state, Effect.NoEffect )
                    -- ( Debug.todo "update the config in the update function", Effect.NoEffect )
                    (
                        Model.LoadedPosts {state | config = applyChanges change state.config}
                        , Effect.NoEffect
                    )
                ( state, _ ) ->
                    ( state, Effect.NoEffect )
    in
    ( { model | state = newState }, cmd )


view : Model -> Html Msg
view model =
    let
        title =
            if model.config.mode == Dev then
                "HackerNews (DEV)"

            else
                "HackerNews"

        body =
            case model.state of
                Model.Empty _ ->
                    div [style "text-align" "center", style "font-size" "20px"] [ text "Loading" ]

                Model.FailedToLoad err ->
                    div [style "color" "red", style "font-weight" "bold", style "font-size" "20px"] [ text <| "Failed to load: " ++ Debug.toString err ]

                Model.LoadedPosts { config, time, posts } ->
                    div []
                        [ postsConfigView config
                        , postTable config time posts
                        ]

                Model.Loading _ ->
                    div [style "text-align" "center"] [ text "Loading stories" ]

                Model.LoadingPosts { currentId } ->
                    div [style "text-align" "center", style "font-size" "20px"] [ text <| "Loading post " ++ String.fromInt currentId ]

                _ ->
                    div [] [ text "Other" ]
    in
    div [ style "font-family" "Arial, sans-serif", style "background-color" "#f2f2f2", style "margin" "0", style "padding" "20px" ]
        [ Html.h1 [ style "color" "#42073f", style "text-align" "center" ] [ text title ]
        , body
        ]
