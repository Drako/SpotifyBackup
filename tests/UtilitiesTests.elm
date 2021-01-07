module UtilitiesTests exposing (..)

import Expect
import List exposing (range)
import Test exposing (Test)
import TestHelpers exposing (parameterized)
import Utilities exposing (hasValue, isAllowedChar, progress)


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
        charsToCases : List Char -> List ( Char, Bool )
        charsToCases =
            List.map (\c -> ( c, True ))

        digits : List ( Char, Bool )
        digits =
            range 0x30 0x39
                |> List.map Char.fromCode
                |> charsToCases

        lowerCase : List ( Char, Bool )
        lowerCase =
            range 97 122
                |> List.map Char.fromCode
                |> charsToCases

        upperCase : List ( Char, Bool )
        upperCase =
            range 65 90
                |> List.map Char.fromCode
                |> charsToCases

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
