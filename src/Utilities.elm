module Utilities exposing (..)

import Char exposing (isAlphaNum)


takeIf : Bool -> a -> Maybe a
takeIf enabled value =
    if enabled then
        Just value

    else
        Nothing


isAllowedChar : Char -> Bool
isAllowedChar c =
    isAlphaNum c || c == ' ' || c == '-' || c == '_'


ifBlank : String -> String -> String
ifBlank default possiblyBlank =
    if String.isEmpty <| String.trim possiblyBlank then
        default

    else
        possiblyBlank


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
