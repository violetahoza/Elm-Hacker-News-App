module Cursor exposing (Cursor, back, current, forward, fromList, length, nonEmpty, toList, withSelectedElement)

{-| Data structure to efficiently navigate a list forward or backward.

It stores a non-empty list as two lists and one element that is currently "selected".

For example, the list `[1, 2, 3]`, when focused on the first element, would be stored as `Cursor [] 1 [2, 3]`.
To focus on the second element, the representation becomes `Cursor [1] 2 [3]`.
Finally, focusing on the third element is: `Cursor [2, 1] 3 []`.

**Note that the left part of the list is stored in reverse order!**

-}

import List as L 

type Cursor a
    = Cursor (List a) a (List a)


-- Creates a cursor with a left list that is reversed, a current element selected and a right list.
withSelectedElement : List a -> a -> List a -> Cursor a
withSelectedElement left mid right =
    Cursor (List.reverse left) mid right


nonEmpty : a -> List a -> Cursor a
nonEmpty x xs =
    Cursor [] x xs


{-| Creates a `Cursor` from a `List`, if the list is not empty

    fromList [ 1, 2, 3 ] --> Just (withSelectedElement [] 1 [2, 3])

    fromList [] --> Nothing

-}
fromList : List a -> Maybe (Cursor a)
fromList list =
    case list of
        [] -> Nothing -- if the list is empty, return Nothing
        x::xs -> Just (nonEmpty x xs) -- otherwise, return a cursor with the first element selected
    -- Nothing
    -- Debug.todo "fromList"


{-| Convert the `Cursor` to a `List`

    toList (nonEmpty 1 [ 2, 3 ]) --> [1, 2, 3]

-}
toList : Cursor a -> List a
toList (Cursor left el right) = L.reverse left ++ (el::right) -- reverse the left list (since it's stored in reverse order) and append the selected element and the right list
    -- []
    -- Debug.todo "toList"


{-| Get the current element from the cursor

    current (nonEmpty 1 [ 2, 3 ]) {- ignore -} --> 1

    current (withSelectedElement [ 1, 2 ] 3 [ 4, 5 ]) {- ignore -} --> 3

-}
current : Cursor a -> a
current (Cursor _ a _) =
    a


{-| Move the cursor forward.

If the cursor would go past the last element, the function should return `Nothing`.

    forward (nonEmpty 1 [ 2, 3 ]) --> Just (withSelectedElement [1] 2 [3])

    forward (nonEmpty 1 []) --> Nothing

    nonEmpty 1 [ 2, 3 ] |> forward |> Maybe.andThen forward --> Just (withSelectedElement [1, 2] 3 [])

    nonEmpty 1 [ 2, 3 ] |> forward |> Maybe.andThen forward |> Maybe.andThen forward {- hidden -} --> Nothing

-}
forward : Cursor a -> Maybe (Cursor a)
forward (Cursor left el right) =
    case right of 
        [] -> Nothing -- if there are no more elements to the right of the selected element, return Nothing
        x::xs -> Just(withSelectedElement (L.reverse(el::left)) x xs) -- otherwise, add the current element to the left list, select the first element of the right list as the new current element, update the right list and return the new cursor
    -- Nothing
    --Debug.todo "forward"


{-| Move the cursor backward.

If the cursor would go before the first element, the function should return `Nothing`.

    back (nonEmpty 1 [ 2, 3 ]) --> Nothing

    back (nonEmpty 1 []) --> Nothing

    nonEmpty 1 [ 2, 3 ] |> forward |> Maybe.andThen back --> Just (withSelectedElement [] 1 [2, 3])

-}
back : Cursor a -> Maybe (Cursor a)
back (Cursor left el right) =
    case left of
        [] -> Nothing -- if there are no elements to the left of the selected element, return Nothing
        x::xs -> Just(withSelectedElement xs x (el::right)) -- otherwise, remove the first element from the left list, make it the new current element, and add the current element to the right list
    -- Nothing
    -- Debug.todo "back"


{-| Get the number of elements

    length (nonEmpty 1 []) --> 1

    length (nonEmpty 1 [ 2, 3 ]) --> 3

-}
length : Cursor a -> Int
length (Cursor left _ right) = L.length left + L.length right + 1 -- sum of the lenghts of left and right lists + 1 (for the selected element)
    -- 0
    -- Debug.todo "length"
