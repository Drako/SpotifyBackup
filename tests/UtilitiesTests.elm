module UtilitiesTests exposing (..)

import Expect
import List exposing (range)
import Test exposing (Test)
import TestHelpers exposing (parameterized)
import Utilities exposing (hasValue, ifBlank, isAllowedChar, progress, takeIf)


takeIfTest : Test
takeIfTest =
    let
        params : List ( Bool, Maybe String )
        params =
            [ ( False, Nothing )
            , ( True, Just "foo" )
            ]
    in
    parameterized "takeIf" params <|
        \( enabled, expected ) -> Expect.equal expected <| takeIf enabled "foo"


ifBlankTest : Test
ifBlankTest =
    let
        params : List ( String, String )
        params =
            [ ( "", "<blank>" )
            , ( " ", "<blank>" )
            , ( "foo", "foo" )
            ]
    in
    parameterized "ifBlank" params <|
        \( input, expected ) -> Expect.equal expected <| ifBlank "<blank>" input


progressTest : Test
progressTest =
    let
        params : List ( List Int, List Int, String )
        params =
            [ ( [], [ 1, 2, 3 ], "1/4" )
            , ( [ 0 ], [ 2, 3 ], "2/4" )
            , ( [ 0, 1 ], [ 3 ], "3/4" )
            , ( [ 0, 1, 2 ], [], "4/4" )
            , ( [], [], "1/1" )
            ]
    in
    parameterized "progress" params <|
        \( done, todo, expected ) -> Expect.equal ("Working on " ++ expected ++ "...") <| progress "Working on" done todo


hasValueTest : Test
hasValueTest =
    let
        params : List ( Maybe Int, Bool )
        params =
            [ ( Just 1, True )
            , ( Nothing, False )
            ]
    in
    parameterized "hasValue" params <|
        \( mb, expected ) -> Expect.equal expected <| hasValue mb


isAllowedCharTest : Test
isAllowedCharTest =
    let
        keyCodesToCases : List Int -> List ( Char, Bool )
        keyCodesToCases =
            List.map Char.fromCode
                >> List.map (\c -> ( c, True ))

        digits : List ( Char, Bool )
        digits =
            range 0x30 0x39
                |> keyCodesToCases

        lowerCase : List ( Char, Bool )
        lowerCase =
            range 97 122
                |> keyCodesToCases

        upperCase : List ( Char, Bool )
        upperCase =
            range 65 90
                |> keyCodesToCases

        params : List ( Char, Bool )
        params =
            [ ( ' ', True )
            , ( '-', True )
            , ( '_', True )
            , ( '#', False )
            , ( ':', False )
            , ( '.', False )
            , ( '{', False )
            , ( '}', False )
            , ( '(', False )
            , ( ')', False )
            , ( '[', False )
            , ( ']', False )
            , ( '/', False )
            , ( '|', False )
            , ( '\\', False )
            , ( '>', False )
            , ( '<', False )
            , ( '&', False )
            , ( '=', False )
            , ( '$', False )
            ]
                ++ digits
                ++ lowerCase
                ++ upperCase
    in
    parameterized "isAllowedChar" params <|
        \( c, isAllowed ) -> Expect.equal isAllowed <| isAllowedChar c
