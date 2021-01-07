module Utilities exposing (..)

import Char exposing (isAlphaNum)


isAllowedChar : Char -> Bool
isAllowedChar c =
    isAlphaNum c || c == ' ' || c == '-' || c == '_'


progress : String -> List a -> List b -> String
progress action done todo =
    let
        doneCount =
            List.length done

        todoCount =
            List.length todo

        current =
            doneCount + 1

        fullCount =
            current + todoCount
    in
    action ++ " " ++ String.fromInt current ++ "/" ++ String.fromInt fullCount ++ "..."


hasValue : Maybe a -> Bool
hasValue mb =
    case mb of
        Just _ ->
            True

        Nothing ->
            False
