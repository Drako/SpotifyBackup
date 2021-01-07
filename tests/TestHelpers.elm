module TestHelpers exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Tuple exposing (first, second)


parameterizedImpl : String -> List a -> (a -> String) -> (a -> b) -> (b -> Expectation) -> Test
parameterizedImpl name testCases titleFunction paramFunction testFunction =
    describe name <|
        List.map
            (\parameter -> test (titleFunction parameter) <| \_ -> testFunction <| paramFunction parameter)
            testCases


parameterized : String -> List a -> (a -> Expectation) -> Test
parameterized name testCases testFunction =
    parameterizedImpl name testCases Debug.toString identity testFunction


parameterizedWithTitles : String -> List ( String, a ) -> (a -> Expectation) -> Test
parameterizedWithTitles name testCases testFunction =
    parameterizedImpl name testCases first second testFunction
