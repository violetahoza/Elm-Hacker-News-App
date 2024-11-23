module Model.Post exposing (..)

import Json.Decode as De
import Time


type alias Post =
    { by : String, id : Int, score : Int, title : String, url : Maybe String, time : Time.Posix, type_ : String }


{-| Decode a `Post`

See: <https://github.com/HackerNews/API#items>

The post is expected to have fields:

  - by: The username of the item's author.
  - id: The item's unique id.
  - score: The story's score.
  - title: The title of the story.
  - url: The URL of the story. **Optional**
  - time: Creation date of the item, in Unix Time.
  - type: The type of item.

_Note_: The `time` field contains the **seconds** since the unix epoch. Take this into consideration when using `Time.millisToPosix`.

Relevant library functions:

  - [Json.Decode.field](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#field)
  - [Json.Decode.string](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#string)
  - [Json.Decode.int](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#int)
  - [Json.Decode.maybe](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#maybe)

-}
decode : De.Decoder Post
decode =
    -- De.fail "TODO"
    -- Debug.todo "Post.decode"
    De.map7 Post -- applies a constructor (Post) to 7 decoded fields; each of the Post fields is extracted one-by-one from the JSON object using De.field
         (De.field "by" De.string) -- extracts the "by" field as a string
         (De.field "id" De.int) -- extracts the "id" field as an integer
         (De.field "score" De.int) -- extracts the "score" field as an integer
         (De.field "title" De.string) -- extracts the "title" field as a string
         (De.maybe (De.field "url" De.string)) -- extracts the "url" field which might be null -> use De.maybe to indicate this
         (De.field "time" (De.map Time.millisToPosix (De.int |> De.map (\x -> x * 1000)))) -- extract the time field: read the integer, multiply it by 1000 (to get the value in ms) and convert the result to Time.Posix
         (De.field "type" De.string) -- extracts the "type" field as a string
