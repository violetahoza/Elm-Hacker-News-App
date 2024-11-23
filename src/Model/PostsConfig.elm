module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Model.Post exposing (Post)
import Time


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString str = case str of 
                        "Score" -> Just Score
                        "Title" -> Just Title
                        "Posted" -> Just Posted
                        "None" -> Just None
                        _ -> Nothing
    -- Nothing
    --Debug.todo "sortFromString"


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change 
    = ChangePostsToShow Int -- update the nr of posts to show
    | ChangeSortBy SortBy -- update the sorting criteria
    | ChangeShowJobPosts Bool -- change the visibility of job posts
    | ChangeShowTextOnlyPost Bool -- change the visibility of text only posts


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change configuration =
    case change of 
        ChangePostsToShow newPostsToShow -> {configuration | postsToShow = newPostsToShow} -- update the nr of posts to show in the configuration
        ChangeSortBy newSortBy -> {configuration | sortBy = newSortBy} -- update the sorting criteria 
        ChangeShowJobPosts isChecked -> {configuration | showJobs = isChecked} -- change the visibility of job posts 
        ChangeShowTextOnlyPost isChecked -> {configuration | showTextOnly = isChecked} -- change the visibility of text only posts 

{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
filterPosts : PostsConfig -> List Post -> List Post
filterPosts configuration posts =
    posts -- start with the complete list of posts
        |> List.filter (\post ->
            (configuration.showTextOnly || post.url /= Nothing) -- only include posts with links when showTextOnly is false
            && (configuration.showJobs || post.type_ /= "job") -- exclude job posts when showJobs is false
        )
        |> List.take configuration.postsToShow -- limit the filtered posts to the number specified in postsToShow
        |> List.sortWith (sortToCompareFn configuration.sortBy) -- sort the limited posts based on the selected SortBy configuration
    -- []
    -- Debug.todo "filterPosts"filterPosts : PostsConfig -> List Post -> List Post

