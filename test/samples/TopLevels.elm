module TopLevels exposing (..)

import Html

type Msg = Inc | Dec

type alias Point = (Int, Int)

main : Html a
main =
    text
        "Hello"
