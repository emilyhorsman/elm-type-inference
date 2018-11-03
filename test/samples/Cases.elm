module Cases exposing (..)

type Msg = Inc | Dec

update message model =
    case message of
        Inc ->
            1

        Dec ->
            True

main =
    text "Hello"
