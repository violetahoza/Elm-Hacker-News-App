module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time
import Html exposing (table)
import Html exposing (thead)
import Html exposing (tr)
import Html exposing (tbody)
import Util.Time exposing (formatTime)
import Html exposing (th)
import Html exposing (td)
import Html exposing (a)
import Html.Attributes exposing (value)
import Html.Attributes exposing (selected)
import Html.Attributes exposing (id)
import Html.Events exposing (onInput)
import Html exposing (option)
import Html exposing (select)
import Html exposing (input)
import Html.Attributes exposing (type_)
import Html.Attributes exposing (checked)
import Html.Events exposing (onCheck)
import Html exposing (label)
import Html.Attributes exposing (for)
import Cursor exposing (current)
import Util.Time exposing (durationBetween)
import Http exposing (post)
import Util.Time exposing (formatDuration)


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
-- Displays a header with columns for score, title, type, posted date, and link.
-- Shows a row for each post with corresponding fields formatted appropriately.
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable configuration currentTime posts =
    div[]
        [table [class "post-table"]
        [
          thead []
              [ tr []
                  [ th [class "post-score"] [text "Score"]
                  , th [class "post-title"] [text "Title"]
                  , th [class "post-type"] [text "Type"]
                  , th [class "post-time"] [text "Posted date"]
                  , th [class "post-url"] [text "Link"]
                  ]
              ]       
          , tbody [] (List.map (postRow currentTime) posts)
        ]
        ]
    -- div [] []
    -- Debug.todo "postTable"

-- Helper function to create a single row in the table for a post.
postRow: Time.Posix -> Post -> Html Msg
postRow currentTime post =
  let
    relativeDuration = case durationBetween post.time currentTime of
                            Just duration -> " (" ++ formatDuration duration ++ ")"
                            Nothing -> ""
  in
    tr []
      [ td [class "post-score"] [text (String.fromInt post.score)]
      , td [class "post-title"] [text post.title]
      , td [class "post-type"] [text post.type_]
      , td [class "post-time"] [text (formatTime Time.utc post.time ++ relativeDuration)]
      , td [class "post-url"] 
          [case post.url of
              Just url -> a [href url] [text "Link"]
              Nothing -> text "No link"
          ]
      ]

{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}

postsConfigView : PostsConfig -> Html Msg
postsConfigView configuration = 
    div [class "posts-config"]
        [ div [class "config-item"]
            [ label [for "select-posts-per-page"] [text "Posts per page: "]
        , select 
            [ id "select-posts-per-page"
            , onInput (\x -> case String.toInt x of
                                Just newPosts -> ConfigChanged (ChangePostsToShow newPosts)
                                Nothing -> ConfigChanged (ChangePostsToShow configuration.postsToShow) -- fallback to current value)
                      )
            ]
            [ option [value "10", selected (configuration.postsToShow == 10)] [text "10"]
            , option [value "25", selected (configuration.postsToShow == 25)] [text "25"]
            , option [value "50", selected (configuration.postsToShow == 50)] [text "50"]
            ]
            ]
        , div [class "config-item"]
            [ label [for "select-sort-by"] [text "Sort by: "]
        , select 
            [ id "select-sort-by"
            , onInput (\x -> ConfigChanged (ChangeSortBy (sortFromString x |> Maybe.withDefault None)))
            ]
            (List.map 
                (\sort -> 
                    option 
                        [ value (sortToString sort)
                        , selected (configuration.sortBy == sort)
                        ] 
                        [text (sortToString sort)]
                ) sortOptions
            )
            ]
        , div [class "config-item"]
            [ label [for "checkbox-show-job-posts"] [text "Show job posts: "]
        , input 
            [ id "checkbox-show-job-posts"
            , type_ "checkbox"
            , checked configuration.showJobs
            , onCheck (\isChecked -> ConfigChanged (ChangeShowJobPosts isChecked))
            ] 
            []
            ]
        , div [class "config-item"]
            [ label [for "checkbox-show-text-only-posts"] [text "Show text-only posts: "]
        , input 
            [ id "checkbox-show-text-only-posts"
            , type_ "checkbox"
            , checked configuration.showTextOnly
            , onCheck (\isChecked -> ConfigChanged (ChangeShowTextOnlyPost isChecked))
            ] 
            []
            ]
        ]