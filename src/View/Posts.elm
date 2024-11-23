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
import Html.Attributes exposing (style)


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
    let
        tableStyle =
            [ class "post-table"
            , style "width" "100%"
            , style "border-collapse" "collapse"
            , style "margin-top" "20px"
            ]
        headerCellStyle =
            [ style "padding" "10px"
            , style "border" "1px solid #ddd"
            , style "color" "#42073f"
            ]
    in
    div[]
        [table tableStyle
        [
          thead [] -- table header row containing column names
              [ tr [style "background-color" "#f9f9f9"]
                  [ th headerCellStyle [text "Score"]
                  , th headerCellStyle [text "Title"]
                  , th headerCellStyle [text "Type"]
                  , th headerCellStyle [text "Posted date"]
                  , th headerCellStyle [text "Link"]
                  ]
              ]       
          , tbody [] (List.map (postRow currentTime) (filterPosts configuration posts)) -- the posts are filtered according to the current configuration.
        ]
        ]
    -- div [] []
    -- Debug.todo "postTable"

-- Helper function to create a single row in the table for a post.
postRow : Time.Posix -> Post -> Html Msg
postRow currentTime post =
    let
        cellStyle =
            [ style "padding" "10px"
            , style "border" "1px solid #ddd"
            ]
    in
    tr [style "border-bottom" "1px solid #ddd"]
        [ td (cellStyle ++ [ class "post-score"])[text (String.fromInt post.score)] -- display the post's score
        , td (cellStyle ++ [ class "post-title"]) [text post.title] -- display the title
        , td (cellStyle ++ [ class "post-type"]) [text post.type_] -- display the type
        , td (cellStyle ++ [ class "post-time"]) -- display the posts's timestamp and relative duration
            [ text 
                (formatTime Time.utc post.time -- format the post's timestamp and append the relative duration 
                    ++ (case durationBetween post.time currentTime of
                            Just duration -> " (" ++ formatDuration duration ++ ")"
                            Nothing -> ""
                       )
                )
            ]
        , td (cellStyle ++ [ class "post-url"]) -- display the post's link if available
            [ case post.url of
                Just url -> a [href url, style "color" "purple"] [text "Link"]
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
    let
        configItemStyle =
            [ style "margin-bottom" "10px" ]

        labelStyle =
            [ style "font-weight" "bold"
            , style "margin-right" "5px"
            , style "color" "#42073f"
            ]

        selectStyle =
            [ style "padding" "5px"
            , style "border" "1px solid #ccc"
            , style "border-radius" "4px"
            , style "background-color" "#fff"
            , style "color" "#333"
            ]
    in
    div [ class "posts-config", style "padding" "10px", style "border" "1px solid #ddd", style "border-radius" "5px", style "background-color" "#f9f9f9" ] -- main container for the configuration options
        [ div configItemStyle -- configuration item for the posts per page setting
            [ label (labelStyle ++ [ for "select-posts-per-page" ]) [ text "Posts per page: " ]
            , select 
                ( [ id "select-posts-per-page"
                  , onInput (\x -> case String.toInt x of
                                      Just newPosts -> ConfigChanged (ChangePostsToShow newPosts) -- change posts per page when user selects a new option
                                      Nothing -> ConfigChanged (ChangePostsToShow configuration.postsToShow)-- if the input is invalid, fallback to current setting
                    ) ]
                ++ selectStyle
                ) 
                [ option [ value "10", selected (configuration.postsToShow == 10) ] [ text "10" ]
                , option [ value "25", selected (configuration.postsToShow == 25) ] [ text "25" ]
                , option [ value "50", selected (configuration.postsToShow == 50) ] [ text "50" ]
                ]
            ]
        , div configItemStyle -- configuration item for sorting options
            [ label (labelStyle ++ [ for "select-sort-by" ]) [ text "Sort by: " ]
            , select 
                ( [ id "select-sort-by"
                  , onInput (\x -> case sortFromString x of
                                      Just sort -> ConfigChanged (ChangeSortBy sort) -- change the sorting criteria based on user input
                                      Nothing -> ConfigChanged (ChangeSortBy None) -- fallback to "None" if an invalid option is selected
                    ) ]
                ++ selectStyle
                ) 
                (List.map 
                    (\sort -> 
                        option 
                            [ value (sortToString sort) -- set the value of each sort option based on its string representation
                            , selected (configuration.sortBy == sort) -- select the current sort option based on the current configuration
                            ] 
                            [ text (sortToString sort) ]
                    ) sortOptions
                )
            ]
        , div configItemStyle -- configuration item for changing job posts visibility
            [ label (labelStyle ++ [ for "checkbox-show-job-posts" ]) [ text "Show job posts: " ]
            , input 
                [ id "checkbox-show-job-posts"
                , type_ "checkbox"
                , checked configuration.showJobs -- check the box if "showJobs" is true in configuration
                , onCheck (\isChecked -> ConfigChanged (ChangeShowJobPosts isChecked)) -- update the config when checkbox is checked/unchecked
                ] 
                []
            ]
        , div configItemStyle -- configuration item for changing text-only posts visibility
            [ label (labelStyle ++ [ for "checkbox-show-text-only-posts" ]) [ text "Show text-only posts: " ]
            , input 
                [ id "checkbox-show-text-only-posts"
                , type_ "checkbox"
                , checked configuration.showTextOnly 
                , onCheck (\isChecked -> ConfigChanged (ChangeShowTextOnlyPost isChecked))
                ] 
                []
            ]
        ]