module Model.PostIds exposing (..)

import Cursor exposing (Cursor)
import Json.Decode as De

-- Represent the types of Hacker News items
type HackerNewsItem
    = Top
    | New
    | Show
    | Ask
    | Jobs


-- Convert HackerNewsItem to a string representation
itemName : HackerNewsItem -> String
itemName item =
    case item of
        Top ->
            "top"

        New ->
            "new"

        Show ->
            "show"

        Ask ->
            "ask"

        Jobs ->
            "job"

-- Represents a list of post IDs, navigable using a Cursor
type PostIds
    = PostIds (Cursor Int)


{-| Returns the first post id

    import Cursor

    first (PostIds (Cursor.nonEmpty 1 [ 2, 3 ])) {- ignore -} --> 1

-}
first : PostIds -> Int
first (PostIds ids) =
    Cursor.current ids -- Use the current field of the Cursor to get the first element


{-| Moves the `Cursor` forward and returns the current post id

If the `Cursor` is focused on the last element, it returns `Nothing`

    import Cursor

    advance (PostIds (Cursor.nonEmpty 1 [ 2, 3 ])) --> Just ( 2, PostIds (Cursor.withSelectedElement [1] 2 [3]))

    advance (PostIds (Cursor.withSelectedElement [ 1, 2 ] 3 [])) --> Nothing

-}
advance : PostIds -> Maybe ( Int, PostIds )
advance (PostIds cursor) =
    case Cursor.forward cursor of -- try to move the cursor to the next post id
        Just nextCursor -> Just ( Cursor.current nextCursor, PostIds nextCursor ) -- if a next cursor exists, return a tuple: the 1st el is the current post id from  the next cursor, and the 2nd el is a new PostIds with the updated cursor
        Nothing -> Nothing -- If there are no more elements, return Nothing
    -- Nothing
    -- Debug.todo "advance"



{-| Returns the first post id

    import Cursor

    first (PostIds (Cursor.nonEmpty 1 [ 2, 3 ])) {- ignore -} --> 1

-}
fromList : List Int -> Maybe PostIds
fromList ids =
    Cursor.fromList ids -- create a cursor from the list of post ids
        |> Maybe.map PostIds -- wrap the cursor in the PostIds type


{-| Decode a list of post ids.

If the list is empty, the function returns `Nothing`.

    import Json.Decode as De
    import Cursor

    De.decodeString decode "[1, 2, 3]" --> Ok (Just (PostIds (Cursor.nonEmpty 1 [2, 3])))

    De.decodeString decode "[1]" --> Ok (Just (PostIds (Cursor.nonEmpty 1 [])))

    De.decodeString decode "[]" --> Ok (Nothing)

-}
decode : De.Decoder (Maybe PostIds)
decode =
    De.list De.int -- Decode a JSON array into a List Int
        |> De.map fromList -- Map the result through fromList to get a Maybe PostIds
    -- De.fail "TODO"
    -- Debug.todo "PostIds.decode"
    -- De.fail "PostIds.decode is not implemented yet"

